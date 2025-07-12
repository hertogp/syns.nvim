--[[ SYNS ]]

local S = {} -- syns module to be returned
local M = {} -- Mythes thesaurus provider
local W = {} -- Wordnet thesaurus provider

--[[ TYPES ]]

---@alias cpos 'a' | 's' | 'v' | 'n' | 'r'
---@alias pos 'adj' | 'adv' | 'verb' | 'noun'

---@class Item
---@field word string word to search for
---@field text string word to search for
---@field words string[] words from all Entry's across all pos's
---@field pos table<pos, Entry>

---@class Entry
---@field pos string part-of-speech this entry came from (index.<pos>)
---@field word string word found in index.<pos>
---@field pointers string[] a list of pointer symbols (chars)
---@field offsets string[] a list of offsets into data.<pos>
---@field syns DataEntry[]

---@class DataEntry
---@field pos string pos where entry was found, from data.<pos>
---@field cpos string pos character of the pos field
---@field words string[] words in this (synonym) data set
---@field gloss string[] definitions and descriptions for this data set
---@field pointers DataPointer[] related data sets this set is pointing to
--@field frames any -- TODO: not actually used, remove?
--@field lexoids any -- TODO: not actually used, remove?

---@class DataPointer
---@field symbol string the pointer data set's relation to pointing data set
---@field relation string name of the relationship
---@field words string[] words from this pointer data set
---@field gloss string[] definitions and descriptions of pointer data set
---@field offset string offset into data.<pos>, where pos is from this pointer
---@field pos string part-of-speech (file extension)
---@field cpos string part-of-speech symbol (character)
---@field sword? string src word(s) in pointing data set, if applicable
---@field dword string pointer data set word(s) related to pointing data set src word(s)
---@field srcnr number index of word in the pointing dataset (0 = all words) TODO: keep?
---@field dstnr number index of related word in this (pointer) data set ( 0 = all words) -- TODO: keep?

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
---@return Entry|nil entry the parsed result
---@return string|nil error message if applicable, nil otherwise
function W.parse_idx(line)
  -- lemma pos synset_cnt p_cnt [symbol...] sense_cnt tagsense_cnt [synset_offset...]
  -- * synset_cnt == sense_cnt (backw.comp.) both skipped, same as #offset (always 1 or more)
  -- * [symbols..] = all the diff kind of relationships with other synsets (see dta pointers)
  local rv = {}
  local parts = vim.split(vim.trim(line), '%s+') -- about 15K idx lines have trailing spaces

  rv.word = parts[1]
  rv.pos = W.cpos_to_ext[parts[2]] -- redundant, is same as pos of the containing index.<pos>

  -- p_cnt [symbol ..]
  local ptr_cnt = tonumber(parts[4]) -- same as #pointers, may be 0
  rv.pointers = {} -- kind of pointers that lemma/term has in all the synsets it is in
  for n = 5, 5 + ptr_cnt - 1 do
    table.insert(rv.pointers, parts[n])
  end

  local ix = 5 + ptr_cnt
  rv.tagsense_cnt = tonumber(parts[ix + 1])
  rv.offsets = {} -- offset into data.<rv.pos> for different senses/meanings of lemma/term
  for n = ix + 2, #parts do
    table.insert(rv.offsets, parts[n])
  end

  return rv, nil
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
    frames = {}, -- frame/word nrs to use in examples sentences (i.e. frames), if any
    lexoids = {}, -- sense-ids used in lexographer file given by Wordnet.lexofile[rv.lexofnr]
  }
  local data = vim.split(line, '|')
  local parts = vim.split(data[1], '%s+', { trimempty = true })
  local gloss = vim.tbl_map(vim.trim, vim.split(data[2], ';%s*'))
  rv.gloss = gloss

  -- skip offset = parts[1]
  rv.lexofnr = tonumber(parts[2]) + 1 -- 2-dig.nr, +1 added for lua's 1-based index in Wordnet.lexofile
  rv.cpos = parts[3]
  rv.pos = W.cpos_to_ext[parts[3]]
  local words_cnt = tonumber(parts[4], 16) -- 2-hexdigits, nr of words in this synset (1 or more)

  -- words = words_cnt x [word lexid]
  local ix = 5
  for i = ix, ix + 2 * (words_cnt - 1), 2 do
    local lemma = parts[i]:gsub('%b()', '') -- case-sensitive, strip the (marker)
    local lexid = tonumber(parts[i + 1], 16)
    table.insert(rv.words, lemma) -- or add word with marker?
    if lexid > 0 then
      table.insert(rv.lexoids, ('%s%s'):format(lemma, lexid)) -- sense-id in lexo-file
    end
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

  -- [frame_cnt x [+ (skipped) frame_nr word_nr (hex)]] -- entire thing is optional!
  ix = ix + ptrs_cnt * 4
  if ix < #parts and pos == 'verb' then
    local frame_cnt = tonumber(parts[ix])
    if frame_cnt and frame_cnt > 0 then
      ix = ix + 1
      for i = ix, ix + 3 * (frame_cnt - 1), 3 do
        table.insert(rv.frames, {
          frame_nr = tonumber(parts[i + 1]),
          word_nr = tonumber(parts[i + 2], 16),
        })
      end
    end
  end

  return rv, nil
end

---reads the entries in data.`pos` for given `offsets`
---@param pos string part of speech
---@param offsets string[] offsets into data.<pos>
---@return table|nil synsets T<offset, dta> as found in data.<pos> or nil for not found or error
---@return string|nil error message in case of an error, nil otherwise
function W.data(pos, offsets)
  local senses = {}
  for _, offset in ipairs(offsets) do
    local line, _, err = binsearch(W.fh.data[pos], offset, '^%S+')
    if err then
      return nil, '[error getting data] ' .. err
    elseif line then
      local dta = W.parse_dta(line, pos)
      if dta then
        -- add gloss and words from synsets pointed to by pointer's offsets and pos
        for _, ptr in ipairs(dta.pointers) do
          local ptr_line, _, err2 = binsearch(W.fh.data[ptr.pos], ptr.offset, '^%S+')
          if not err2 and ptr_line then
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
        senses[offset] = dta -- note: dta might be nil
      end

      --
    else
      vim.notify(('nothing found for offset %s in data.%s'):format(offset, pos))
      return nil, nil
    end
  end

  return senses, nil
end

---searches the thesaurus for given `word`, returns its item or nil
---@param word string word or collocation to lookup in the thesaurus
---@return table|nil item thesaurus results for given `word`, nil if not found
---@return string|nil error message or nil for no error
function W.search(word)
  W.open()
  local item = { word = word, pos = {} }
  local words = {}

  for _, pos in ipairs(W.pos) do
    -- search word in all index.<pos>-files, ignore abort's if binsearch lands on
    -- license text at the start of the <pos>-file.
    local line, _, _ = binsearch(W.fh.index[pos], word, '^%S+')

    if line then
      -- found an entry in one of the index.<pos> files
      local idx, err_idx = W.parse_idx(line)
      if idx then
        idx.word = word
        local syns = W.data(pos, idx.offsets) -- get dta sense entries
        idx.syns = syns or {} -- syns might be nil
        item.pos[pos] = idx

        -- build words, collect from senses and its pointers
        -- remove any potential markers from words and lowercase them
        -- TODO: idx.syns should always exist, right?  no `or {}` needed
        for _, syn in pairs(idx.syns) do
          for _, w in ipairs(syn.words) do
            local new = w:gsub('%b()$', ''):lower()
            words[new] = true
          end
          for _, ptr in ipairs(syn.pointers) do
            for _, w in ipairs(ptr.words or {}) do
              local new = w:gsub('%b()$', ''):lower()
              words[new] = true
            end
          end
        end
      elseif err_idx then
        vim.notify('[error] parsing index line: ' .. err_idx, vim.log.levels.ERROR)
      end
    end
  end

  -- collection of words found in synsets
  words = vim.tbl_keys(words)

  if #words > 0 then
    item.text = word -- used by snack matcher
    table.sort(words) -- sorts in-place
    item.words = words
  else
    item = nil
  end

  W.close()
  return item, nil
end

--[[ SYNS MODULE ]]

function S.test()
  local items = W.search('happy')
  vim.print(vim.inspect(items))
end

return S
