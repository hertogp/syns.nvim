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
-- -@field pos string pos of data.<pos> where this synset was found
--@field frames any -- TODO: not actually used, remove?
--@field lexoids any -- TODO: not actually used, remove?

---@class Pointer
---@field relation string name of the relationship
---@field words string[] words from this dst (pointer) data set
---@field gloss string[] definitions and descriptions of dst data set
---@field pos string pos of data.<pos> where this dst data set was found
---@field offset string offset into data.<pos>, where this dst data set was found
---@field sword? string 0+ words of src data set to which this relationship pertains
---@field dword string 1+ words of dst data set related to sword?
-- -@field srcnr number index of word in the pointing dataset (0 = all words) --TODO: keep?
-- -@field dstnr number index of related word in this (pointer) data set ( 0 = all words) -- TODO: keep?
-- -@field symbol string type of relation of this dst data set to its src data set
-- -@field cpos string pos symbol (character) for pos from data.<pos>

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
  for _, fstem in ipairs({ 'index', 'data' }) do
    W.fh[fstem] = W.fh[fstem] or {}
    for _, cpos in ipairs({ 'a', 'v', 'n', 'r' }) do
      local fext = W.cpos_to_ext[cpos]
      if W.fh[fstem][fext] == nil then
        W.fh[fstem][fext], err = io.open(syns_fname('wordnet', fstem, fext), 'r')
        assert(W.fh[fstem][fext], err)
      end
    end
  end
end

--- closes all wordnet open file handles stored in `W.fh[fstem][fext]`
function W.close()
  for fstem, t in pairs(W.fh) do
    for fext, fh in pairs(t) do
      fh:close()
      W.fh[fstem][fext] = nil
    end
    W.fh[fstem] = nil
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
---@param pos string part-of-speech where `line` came from (data.<pos>)
---@return table|nil result table with parsed fields; nil on error
---@return string|nil error message if applicable, nil otherwise
function W.parse_dta(line, pos)
  -- offset lexofnr ss_type w_cnt word lexid [word lexid ..] p_cnt [ptr...] [frames...] | gloss
  -- [frames] only for pos==verb and entirely optional

  -- TODO: check line, pos are not nil or pos invalid
  if line == nil then
    return nil, '[error] input line is nil'
  end
  local rv = {
    words = {}, -- synset-words
    pointers = {}, -- specific relations with words in other synsets
  }
  local data = vim.split(line, '|')
  local parts = vim.split(data[1], '%s+', { trimempty = true })
  local gloss = vim.tbl_map(vim.trim, vim.split(data[2], ';%s*'))
  rv.gloss = gloss

  rv.cpos = parts[3]
  rv.pos = W.cpos_to_str[parts[3]]
  local words_cnt = tonumber(parts[4], 16) -- 2-hexdigits, nr of words in this synset (1 or more)

  -- words = words_cnt x [word lexid]
  local ix = 5
  for i = ix, ix + 2 * (words_cnt - 1), 2 do
    local lemma = parts[i]:gsub('%b()', '') -- case-sensitive, strip the (marker)
    table.insert(rv.words, lemma) -- or add word with marker?
  end

  -- pointers = ptr_count x [{symbol, synset-offset, pos-char, src|tgt hex numbers}, ..]
  ix = 5 + 2 * words_cnt
  local ptrs_cnt = tonumber(parts[ix]) -- 3-digit nr, ptrs to other synsets
  ix = ix + 1
  for i = ix, ix + (ptrs_cnt - 1) * 4, 4 do
    local symbol = parts[i]
    if W.pointers_keep[symbol] then
      local srcnr, dstnr = parts[i + 3]:match('^(%x%x)(%x%x)')
      srcnr = tonumber(srcnr, 16)
      dstnr = tonumber(dstnr, 16)
      table.insert(rv.pointers, {
        symbol = symbol,
        relation = W.pointers_keep[symbol] or ('unknown ' .. symbol),
        offset = parts[i + 1],
        cpos = parts[i + 2], -- used by preview to map cpos via cpos_to_str (preserves adj-satellite)
        pos = W.cpos_to_ext[parts[i + 2]] or parts[i + 2], -- maps s to adj, not adj-satellite
        srcnr = srcnr,
        dstnr = dstnr,
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
  if W.fh.data[pos] == nil then
    return nil, ('[error] no data.%s found!'):format(pos)
  end

  local offs = tonumber(offset)
  W.fh.data[pos]:seek('set', offs)
  local line = W.fh.data[pos]:read('*l')
  if line then
    local dta = W.parse_dta(line, pos)
    if dta then
      -- add gloss and words from synsets pointed to by pointer's offsets and pos
      for _, ptr in ipairs(dta.pointers) do
        local ptr_offset = tonumber(ptr.offset)
        W.fh.data[ptr.pos]:seek('set', ptr_offset)
        local ptr_line = W.fh.data[ptr.pos]:read('*l')
        if ptr_line then
          local ptr_dta, err_dta = W.parse_dta(ptr_line, dta.pos)
          if not err_dta and ptr_dta then
            ptr.gloss = ptr_dta.gloss
            ptr.words = ptr_dta.words
            ptr.dword = ptr.dstnr == 0 and table.concat(ptr.words, ', ') or ptr.words[ptr.dstnr]
            ptr.sword = ptr.srcnr > 0 and dta.words[ptr.srcnr] or nil
          else
            ptr.gloss = {}
            ptr.words = {}
          end
        end
      end
      return dta, nil
    end
  else
    local msg = ('[error] no line found at offset %s'):format(offset)
    vim.notify(msg, vim.log.levels.ERROR)
  end

  return nil, nil
end

---searches the thesaurus for given `word`, returns its item or nil
---@param word string word or collocation to lookup in the thesaurus
---@return table|nil item thesaurus results for given `word`, nil if not found
---@return string|nil error message or nil for no error
function W.search(word)
  W.open()
  word = word:gsub(' ', '_'):lower()
  local item = { word = word, pos = {} }

  for _, pos in ipairs(W.pos) do
    local line, _, _ = binsearch(W.fh.index[pos], word, '^%S+')

    if line then
      local dta, err_idx = W.parse_idx(line)
      if err_idx then
        local msg = ('[error] %s, for %s'):format(err_idx or '?', line)
        vim.notify(msg, vim.log.levels.ERROR)
      else
        item.pos[pos] = dta
      end
    end -- no line, nothing found for this pos in W.pos
  end

  W.close()
  return item, nil
end

--[[ SYNS MODULE ]]

function S.test()
  local mt = {
    all = function(self)
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

    oops = 21,
  }
  mt.__index = mt
  local item = W.search('cum laude')
  if item == nil then
    return
  end
  item = setmetatable(item, mt)
  local words = item:all()
  vim.print(vim.inspect({ 'outer', item:all() }))
  vim.print('got ' .. #words .. ' words')
  vim.print(vim.inspect(item))
end

return S
