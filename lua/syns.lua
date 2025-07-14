--[[ SYNS ]]

local S = {} -- syns module to be returned
local M = {} -- Mythes thesaurus provider
local W = {} -- Wordnet thesaurus provider

--[[ TYPES ]]

---@alias cpos 'a' | 's' | 'v' | 'n' | 'r'
---@alias pos 'adj' | 'adv' | 'verb' | 'noun'

---@class Item
---@field word string word to search for
---@field pos table<pos, Synset[]>

---@class Synset
---@field cpos string pos character a|s|v|n|r (s=adj-satellite)
---@field words string[] words in this synset (data) set
---@field gloss string[] definitions and descriptions for this data set
---@field pointers Pointer[] pointers to related data (dst) sets for this (src) set

---@class Pointer
---@field cpos string symbol (character) for pos in data.<pos>
---@field relation string name of the relationship between src and dst data set
---@field words string[] words from this dst (pointer) data set
---@field gloss string[] definitions and descriptions of dst data set
---@field sword? string 0+ words of src data set to which this relationship pertains
---@field dword string 1+ words of dst data set related to sword

--[[ LOCALS ]]

local ns_tsr = vim.api.nvim_create_namespace('ns_thesaurus')
local hl_tsr = {
  text = 'Special',
  word = 'Special',
  number = 'Number',
  pos = 'Comment',
  relation = 'Constant',
  trivial = 'Comment',
  pointer = 'Keyword',
}

--[[ HELPERS ]]

---binary search for word in an ordered (thesaurus) index
---@param file any filehandle of file to be searched
---@param word string to search for in the given `file`
---@param mexpr string a string.match expression to extract word from line for comparison
---@return string|nil line found in the file for given `word`, nil for not found
---@return number offset to last line read while searching (so not necessarily matched)
---@return string|nil error message or nil for no error
local function binsearch(file, word, mexpr)
  local line
  -- TODO: `aaaa` ends up in the license text, which has spaces
  local p0, p1, err = 0, file:seek('end', 0)
  if err then
    return nil, 0, err
  end

  while p0 <= p1 do
    local pos = file:seek('set', math.floor((p0 + p1) / 2))
    _ = file:read('*l') -- discard (remainder) of current line
    line = file:read('*l') -- read next available line

    -- p0...[discard\nline\n]...p1 --
    ---------^= pos----------^= cur

    local entry = line:match(mexpr)
    if entry == nil then
      return nil, file:seek('cur') - #line - 1, ('[error] expr %s, invalid input %q '):format(mexpr, line)
    elseif word < entry then
      p1 = pos - 1 -- term < line, move p1 to just before the start of discard (i.e. always move left)
    elseif word > entry then
      p0 = file:seek('cur') - 1 --  term > line, move p0 to \n of last line read
    else
      -- word == entry, so found it: return line, offset to start-of-line and 'no error'
      return line, file:seek('cur') - #line - 1, nil
    end
  end

  -- nothing found, so return nil, offset to last line read and no err msg
  return nil, file:seek('cur') - #line - 1, nil
end

---returns filename for a thesaurus file id'd by `subdir`, `fstem` and `fext`
---@param thesaurus string the thesaurus name: a subdir of syns' dta dir that holds its files
---@param fstem string filename sans extension of a thesaurus file
---@param fext string file extension of a thesaurus file
---@return string path full pathname of a thesaurus file
local function syns_fname(thesaurus, fstem, fext)
  local topdir = debug.getinfo(1, 'S').source:sub(2, -1)
  local dtadir = vim.fs.joinpath(vim.fn.fnamemodify(topdir, ':p:h:h'), 'dta', thesaurus)
  return vim.fs.joinpath(dtadir, ('%s.%s'):format(fstem, fext))
end

--[[ WORDNET ]]

W = {
  name = 'Wordnet',

  pos = { 'adj', 'adv', 'verb', 'noun' }, -- part of speech

  cpos_to_ext = {
    -- maps part-of-speech character to file extension: {index, data}.<pos>
    a = 'adj',
    s = 'adj', -- adjective-satellite
    v = 'verb',
    n = 'noun',
    r = 'adv',
  },

  cpos_to_str = {
    -- maps part-of-speech character symbol to display name
    a = 'adjective',
    s = 'adj-satellite',
    v = 'verb',
    n = 'noun',
    r = 'adverb',
  },

  fh = {}, -- filehandles indexed by [fstem][fext]

  pointers = {
    ['!'] = 'Antonym',
    ['#m'] = 'Member holonym',
    ['#p'] = 'Part holonym',
    ['#s'] = 'Substance holonym',
    ['%m'] = 'Member meronym',
    ['%p'] = 'Part meronym',
    ['%s'] = 'Substance meronym',
    ['&'] = 'Similar to',
    ['+'] = 'Derivationally related form',
    ['-c'] = 'Member of this domain - TOPIC',
    ['-r'] = 'Member of this domain - REGION',
    ['-u'] = 'Member of this domain - USAGE',
    [';c'] = 'Domain of synset - TOPIC',
    [';r'] = 'Domain of synset - REGION',
    [';u'] = 'Domain of synset - USAGE',
    ['='] = 'Attribute',
    ['@'] = 'Hypernym',
    ['@i'] = 'Instance Hypernym',
    ['~'] = 'Hyponym',
    ['~i'] = 'Instance Hyponym',
    ['*'] = 'Entailment',
    ['^'] = 'Also see',
    ['<'] = 'Participle of verb',
    ['\\'] = 'Pertainym (pertains to noun)',
  },

  pointers_keep = {
    ['!'] = 'antonym',
    ['&'] = 'similar',
    ['^'] = 'see-also',
    ['+'] = 'related', -- Derivationally related
    ['*'] = 'entailment',
    ['\\'] = 'pertains-to',
  },
}

--- opens all wordnet files storing fh's in `W.fh[fstem][fext]`
function W.open()
  local err
  for _, stem in ipairs({ 'index', 'data' }) do
    W.fh[stem] = W.fh[stem] or {}
    for _, cpos in ipairs({ 'a', 'v', 'n', 'r' }) do
      local ext = W.cpos_to_ext[cpos]
      if W.fh[stem][ext] == nil then
        W.fh[stem][ext], err = io.open(syns_fname('wordnet', stem, ext), 'r')
        assert(W.fh[stem][ext], err)
      end
    end
  end
end

--- closes all wordnet open file handles stored in `W.fh[fstem][fext]`
function W.close()
  for stem, t in pairs(W.fh) do
    for ext, fh in pairs(t) do
      fh:close()
      W.fh[stem][ext] = nil
    end
    W.fh[stem] = nil
  end
end

---parse a line from index.<pos>; returns IndexEntry or nil if not found
---@param line string as found in an index.<pos> file
---@return Synset[]|nil entry the parsed result
---@return string|nil error message if applicable, nil otherwise
function W.parse_idx(line)
  -- lemma pos synset_cnt p_cnt [symbol...] sense_cnt tagsense_cnt [synset_offset...]
  -- see `:Open https://wordnet.princeton.edu/documentation/wndb5wn`
  local synsets = {}
  local parts = vim.split(vim.trim(line), '%s+') -- about 15K idx lines have trailing spaces

  local pos = W.cpos_to_ext[parts[2]]

  local ptr_cnt = tonumber(parts[4]) -- may be 0
  for n = 5 + ptr_cnt + 2, #parts do
    local dta = W.data(pos, parts[n])
    table.insert(synsets, dta)
  end

  return synsets, nil
end

---parses a data.<pos> line into table
---@param line string the data.<pos> entry to be parsed
-- -@param pos string part-of-speech where `line` came from (data.<pos>)
---@return table|nil result table with parsed fields; nil on error
---@return string|nil error message if applicable, nil otherwise
function W.parse_dta(line, _) -- _ = pos, if pos=verb you might have frames
  -- offset lexofnr ss_type w_cnt [word lexid ..] p_cnt [ptr...] [frames...] | gloss

  if line == nil then
    return nil, '[error] input line is nil'
  end

  local rv = {
    words = {}, -- words of this synset
    pointers = {}, -- relationships with words in other synsets
  }

  local data = vim.split(line, '|') -- parts | gloss
  local parts = vim.split(data[1], '%s+', { trimempty = true })

  rv.gloss = vim.tbl_map(vim.trim, vim.split(data[2], ';%s*'))
  rv.cpos = parts[3]
  rv.pos = W.cpos_to_str[parts[3]]

  -- words = words_cnt x [word lexid]
  local w_cnt = tonumber(parts[4], 16) -- 2-hexdigits, nr of words in this synset (1 or more)
  local ix = 5
  for i = ix, ix + 2 * (w_cnt - 1), 2 do
    local word = parts[i]:gsub('%b()', '') -- case-sensitive; strip the (marker)
    table.insert(rv.words, word)
  end

  -- pointers = ptr_count x [{symbol, synset-offset, pos-char, src|tgt hex numbers}, ..]
  ix = 5 + 2 * w_cnt
  local p_cnt = tonumber(parts[ix]) -- 3-digit nr, ptrs to other synsets
  ix = ix + 1
  for i = ix, ix + (p_cnt - 1) * 4, 4 do
    local symbol = parts[i]
    if W.pointers_keep[symbol] then
      local srcnr, dstnr = parts[i + 3]:match('^(%x%x)(%x%x)')
      table.insert(rv.pointers, {
        symbol = symbol,
        relation = W.pointers_keep[symbol],
        offset = parts[i + 1], -- into data.<W.cpos_to_ext[cpos]>
        cpos = parts[i + 2], -- pos symbol of pointer data set
        srcnr = tonumber(srcnr, 16),
        dstnr = tonumber(dstnr, 16),
      })
    end
  end

  return rv, nil
end

---reads the Synset (including its pointers) from data.`pos` at given `offset`
---@param pos string part of speech
---@param offset string offsets into data.<pos>
---@return Synset|nil synset a parsed entry found data.<pos> at `offset`; nil for not found or error
---@return string|nil error message in case of an error, nil otherwise
function W.data(pos, offset)
  assert(W.fh.data[pos], ('[error] file "data.%s" not open/available'):format(pos))

  local offs = tonumber(offset)
  W.fh.data[pos]:seek('set', offs)
  local line = W.fh.data[pos]:read('*l')
  local dta = W.parse_dta(line, pos)

  if dta == nil then
    local msg = '[error] parsing failed: file %s, offset %s, line "%s"'
    return nil, msg:format('data.' .. pos, offset, line)
  end

  -- enrich pointers (if any)
  for _, ptr in ipairs(dta.pointers) do
    local ptr_offset = tonumber(ptr.offset)
    local ptr_pos = W.cpos_to_ext[ptr.cpos]
    W.fh.data[ptr_pos]:seek('set', ptr_offset)
    local ptr_line = W.fh.data[ptr_pos]:read('*l')
    local ptr_dta = W.parse_dta(ptr_line, dta.pos)

    -- add new fields (sword only if subset of dta.words is applicable)
    ptr.gloss = ptr_dta and ptr_dta.gloss or {}
    ptr.words = ptr_dta and ptr_dta.words or {}
    ptr.dword = ptr.dstnr == 0 and table.concat(ptr.words, ', ') or ptr.words[ptr.dstnr]
    ptr.sword = ptr.srcnr > 0 and dta.words[ptr.srcnr] or nil

    -- del old fields (no longer used)
    ptr.offset = nil
    ptr.symbol = nil
    ptr.srcnr = nil
    ptr.dstnr = nil
  end

  return dta, nil
end

---searches the thesaurus for given `word`, returns its Item or nil
---@param word string word or collocation to lookup in the thesaurus
---@return Item|nil item thesaurus Item for given `word`, nil if not found
function W.search(word)
  W.open()
  word = word:gsub(' ', '_'):lower() -- ensure collocation, if applicable
  local item = { word = word, pos = {} }

  for _, pos in ipairs(W.pos) do
    local line, _, _ = binsearch(W.fh.index[pos], word, '^%S+')

    if line then
      local dta, _ = W.parse_idx(line) -- ignore errors
      item.pos[pos] = dta -- nb: dta is nil if not found
    end -- no line, nothing found for this pos in W.pos
  end

  W.close()

  if vim.fn.empty(item.pos) == 1 then
    return nil
  end
  return item
end

--[[ SYNS MODULE ]]

function S.test()
  local mt = {
    words = function(self)
      local words = { [self.word] = true }
      for _, pos in pairs(self.pos) do
        -- words[pos.word] = true
        for _, synset in ipairs(pos) do
          for _, word in ipairs(synset.words) do
            words[word] = true
          end
          for _, ptr in ipairs(synset.pointers) do
            for _, word in ipairs(ptr.words) do
              words[word] = true
            end
          end
        end
      end
      words = vim.tbl_keys(words)
      table.sort(words)
      return words
    end,

    meanings = function(self)
      local count = 0
      for _, pos in pairs(self.pos) do
        count = count + #pos
      end
      return count
    end,
  }
  mt.__index = mt
  local item = W.search('make')
  if item == nil then
    return
  end
  item = setmetatable(item, mt)
  local words = item:words()
  vim.print(vim.inspect({ 'item', #words, words }))
  vim.print(vim.inspect({ 'meanings', item:meanings() }))

  vim.print(vim.inspect(item))

  -- vim.ui.select() - an alternative to snacks.picker, in case of no snacks ?
  -- there are a lot of plugins (incl. snacks) that override this function with
  -- something of their own ..
end

return S
