--[[ SYNS ]]

local S = {} -- syns module to be returned
local M = {} -- Mythes thesaurus provider
local W = {} -- Wordnet thesaurus provider

--[[ TYPES ]]

---@alias cpos 'a' | 's' | 'v' | 'n' | 'r'
---@alias pos 'adj' | 'adv' | 'verb' | 'noun'

---@class Item
---@field word string word to search for
---@field synsets Synset[]

---@class Synset
---@field cpos string pos character a|s|v|n|r (s=adj-satellite)
---@field words string[] words in this synset (data) set
---@field gloss string[] definitions and descriptions for this data set
---@field pointers Pointer[] pointers to related data (dst) sets for this (src) set

---@class Pointer
---@field cpos string symbol (character) for pos in data.<pos> where pointer was found
---@field relation string name of the relationship between src and dst data set
---@field srcnr number 0 means all synset words or word at srcnr index
---@field dstnr number 0 means all pointer words or word at dstnr index
---@field words string[] words from this dst (pointer) data set
---@field gloss string[] definitions and descriptions of dst data set

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
    ['&'] = 'Similar to',
    ['^'] = 'Also see',
    ['+'] = 'Derivationally related form',
    ['*'] = 'Entailment',
    ['\\'] = 'Pertainym (pertains to noun)',
    --
    ['#m'] = 'Member holonym',
    ['#p'] = 'Part holonym',
    ['#s'] = 'Substance holonym',
    ['%m'] = 'Member meronym',
    ['%p'] = 'Part meronym',
    ['%s'] = 'Substance meronym',
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
    ['<'] = 'Participle of verb',
  },

  pointers_keep = {
    ['!'] = 'antonym',
    ['&'] = 'similar',
    ['^'] = 'see also',
    ['+'] = 'related',
    ['*'] = 'entailment',
    ['\\'] = 'pertains-to',
    --
    -- ['#m'] = 'Member holonym',
    -- ['#p'] = 'Part holonym',
    -- ['#s'] = 'Substance holonym',
    -- ['%m'] = 'Member meronym',
    -- ['%p'] = 'Part meronym',
    -- ['%s'] = 'Substance meronym',
    -- ['-c'] = 'Member of this domain - TOPIC',
    -- ['-r'] = 'Member of this domain - REGION',
    -- ['-u'] = 'Member of this domain - USAGE',
    -- [';c'] = 'Domain of synset - TOPIC',
    -- [';r'] = 'Domain of synset - REGION',
    -- [';u'] = 'Domain of synset - USAGE',
    -- ['='] = 'Attribute',
    -- ['@'] = 'Hypernym',
    -- ['@i'] = 'Instance Hypernym',
    -- ['~'] = 'Hyponym',
    -- ['~i'] = 'Instance Hyponym',
    -- ['<'] = 'Participle of verb',
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
    local relation = W.pointers_keep[parts[i]] -- symbol to keep
    if relation then
      local srcnr, dstnr = parts[i + 3]:match('^(%x%x)(%x%x)')
      table.insert(rv.pointers, {
        relation = relation,
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

    -- del old fields (no longer used)
    ptr.offset = nil
  end

  return dta, nil
end

---searches the thesaurus for given `word`, returns its Item or nil
---@param word string word or collocation to lookup in the thesaurus
---@return Item|nil item thesaurus Item for given `word`, nil if not found
function W.search(word)
  W.open()
  word = word:gsub(' ', '_'):lower() -- ensure collocation, if applicable
  local item = { word = word, synsets = {} }

  for _, pos in ipairs(W.pos) do
    local line, _, _ = binsearch(W.fh.index[pos], word, '^%S+')
    if line then
      local synsets = W.parse_idx(line) or {}
      for _, synset in ipairs(synsets) do
        table.insert(item.synsets, synset)
      end
    end
  end

  W.close()

  if #item.synsets == 0 then
    return nil
  end
  return item
end

--[[ SYNS MODULE ]]

function S.test(word)
  local mt = {
    iter_words = function(self, cb)
      local ptrs = {}
      for _, synset in ipairs(self.synsets) do
        for _, ptr in ipairs(synset.pointers) do
          local swords = ptr.srcnr == 0 and synset.words or { synset.words[ptr.srcnr] }
          local dwords = ptr.dstnr == 0 and ptr.words or { ptr.words[ptr.dstnr] }
          for _, sw in ipairs(swords) do
            for _, dw in ipairs(dwords) do
              local pos = W.cpos_to_str[ptr.cpos]
              local id = ('%s,%s,%s,%s'):format(dw, pos, ptr.relation, sw)
              if not ptrs[id] then
                cb(dw, pos, ptr.relation, sw)
                ptrs[id] = true
              end
            end
          end
        end
      end
      vim.print(vim.inspect(ptrs))
    end,
  }

  mt.__index = mt
  local item = W.search(word)
  if item == nil then
    return
  end
  item = setmetatable(item, mt)

  -- item:iter_words(function(w, t, r, s)
  --   vim.print(('%s, %s, %s, %s'):format(w, t, r, s))
  -- end)

  local seen = {}
  for _, set in ipairs(item.synsets) do
    for _, sword in ipairs(set.words) do
      sword = sword:gsub('_', ' ')
      local pos = W.cpos_to_str[set.cpos]
      local rv = ('%-15s| %s, %s'):format(sword, pos, item.word)
      if not seen[rv] then
        vim.print(rv)
        seen[rv] = true
      end
    end
    for _, ptr in ipairs(set.pointers) do
      -- vim.print(('ptr: %s'):format(table.concat(ptr.words, ', ')))

      local pos = W.cpos_to_str[ptr.cpos]
      for ix, dword in ipairs(ptr.words) do
        dword = dword:gsub('_', ' ')
        local sword = set.words[ptr.srcnr]
        sword = sword and sword:gsub('_', ' ')
        local eg = ptr.gloss[1]
        local rv
        if sword and ix == ptr.dstnr then
          rv = ('%-15s| %s, %s, %s - %s'):format(dword, pos, ptr.relation, sword, eg)
        else
          rv = ('%-15s| %s, %s'):format(dword, pos, eg)
        end
        if not seen[rv] then
          vim.print(rv)
        else
          vim.print('--' .. rv)
        end
        seen[rv] = true
      end
    end
  end

  -- vim.print(vim.inspect(item))

  -- local choices = {}
  -- item:iter_words(function(word, pos, relation)
  --   -- if relation then
  --   choices[{ word, pos, relation }] = true
  --   -- end
  -- end)
  --
  -- vim.ui.select(vim.tbl_keys(choices), {
  --   prompt = 'Thesaurus: ' .. search,
  --   format_item = function(c)
  --     -- {word, pos, relation}
  --     local text = ('%-15s | %-10s | %s'):format(c[1], c[3], c[2])
  --     return text
  --   end,
  -- }, function(choice, idx)
  --   vim.print('you choose ' .. (idx or 0) .. ': ' .. (vim.inspect(choice)))
  -- end)
  -- vim.print(vim.inspect(item))
end

return S
