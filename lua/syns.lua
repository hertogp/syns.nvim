--[[ SYNS ]]

local S = {} -- syns module to be returned
local M = {} -- Mythes thesaurus provider
local W = {} -- Wordnet thesaurus provider
local P = {} -- snacks picker functions

--[[ TYPES ]]

---@alias cpos 'a' | 's' | 'v' | 'n' | 'r'
---@alias pos 'adj' | 'adv' | 'verb' | 'noun'

---@class Item
---@field word string `word `to search for
---@field synsets Synset[] synonym sets found for given `word`
---@field words fun(item: Item, cb: function) iterate over `words` found

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

local mt = {}

---iterates over words in Item, calling cb for each word
---@param self Item
---@param cb fun(word: string, pos: string, gloss:string[], srcword:string, relation: string|nil)
function mt.words(self, cb)
  -- cb(dst_word, pos, gloss, src_word, relation)
  --
  local seen = {}
  local v = ''
  for _, synset in ipairs(self.synsets) do
    -- w  -(s)-> synset words
    -- 1. synset words, are synonyms for item.word
    for _, dword in ipairs(synset.words) do
      dword = dword:gsub('_', ' ')
      local sword = self.word:gsub('_', ' ')
      local pos = W.cpos_to_str[synset.cpos] or synset.cpos
      local id = ('%s,%s,%s,%s'):format(dword, pos, sword, 'synonym')
      if not seen[id] then
        cb(dword, pos, synset.gloss, self.word, 'synonym')
        seen[id] = v
      end
    end

    -- 2. synset pointers, other synsets with words related to this synset
    -- src/dstnr = 0 means all of src/dst synset words, so just use the first word
    for _, ptr in ipairs(synset.pointers) do
      -- w  <-(s)- synset words  <-(?)-  pointer synset' words
      -- ^-:-:-:-:-:-:-:-:- ^-x----(?)------------------y-^
      local pos = W.cpos_to_str[ptr.cpos] or ptr.cpos
      local srcnr = ptr.srcnr == 0 and 1 or ptr.srcnr
      local sword = synset.words[srcnr] or self.word
      sword = sword:gsub('_', ' ')

      local words = ptr.dstnr == 0 and ptr.words or { ptr.words[ptr.dstnr] }
      for _, dword in ipairs(words) do
        dword = dword:gsub('_', ' ')
        local id = ('%s,%s,%s,%s'):format(dword, pos, sword, ptr.relation)
        if not seen[id] then
          cb(dword, pos, ptr.gloss, sword, ptr.relation)
        end
        seen[id] = v
      end
    end
  end
end

mt.__index = mt

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

--[[ MYTHES ]]
-- see `:Open https://github.com/hunspell/mythes`
-- see `:Open https://github.com/hunspell/mythes/blob/master/data_layout.txt`

M = {
  name = 'Mythes',

  fh = {},
}

--- opens the Mythes .idx/.dat files, returns true for success, false otherwise
---@return boolean success
---@return string|nil error
function M.open()
  for _, ext in ipairs({ 'idx', 'dat' }) do
    if M.fh[ext] == nil then
      local fname = syns_fname('mythes', 'th_en_US_v2', ext)
      local err
      M.fh[ext], err = io.open(fname, 'r')
      assert(M.fh[ext], err)
    end
  end

  return true, nil
end

---close the mythes .idx/.dat file, always returns true
function M.close()
  for _, ext in ipairs({ 'idx', 'dat' }) do
    if M.fh[ext] then
      M.fh[ext]:close()
      M.fh[ext] = nil
    end
  end
  return true
end

---read thesaurus data entry at given `offset`
---@param offset number offset to data entry
---@return table entry data entry found (if any) {term=term, syns={ {(pos), syn1, ..}, ..} }
---@return string|nil err message in case of errors, nil otherwise
function M.data(offset)
  -- dat-format:
  -- 1. term|num_lines
  -- 2. (pos)|syn1|syn2.. (x num_lines)
  --     * syn_x = word(s) [(relation)]
  local file = M.fh.dat
  local line, err

  if file == nil then
    return {}, '[error] dta filehandle not available'
  end

  _, err = file:seek('set', offset)
  if err then
    return {}, err
  end

  -- 1. term|num_lines
  line = file:read('*l')
  local term, nlines = line:match('([^|]+)|(%d+)$')
  nlines = tonumber(nlines)

  -- 2. (pos)|syn1|syn2..
  local syns = {} -- synset[] found

  if term and nlines then
    for _ = 1, nlines do
      line, err = file:read('*l')
      if err then
        return {}, err
      end

      local fields = vim.split(line, '|', { plain = true, trimempty = true })
      fields = vim.tbl_map(vim.trim, fields)

      -- create Synset and pointers
      local pos = fields[1]:gsub('[()]', '')
      local relation = {} -- relation->words, rel='' are actual synonyms
      for ix = 2, #fields do
        -- e.g.: 'word (generic term)' -> rel = generic
        local field = fields[ix]
        -- local rel = (field:match('%b()') or ''):match('%S*'):sub(2, -1)
        -- local word = field:match('^[^(]+'):gsub('%s*$', '')
        local word, rel = unpack(vim.split(field, '%s*%('))
        rel = vim.split(rel or '', '%s')[1]
        if relation[rel] then
          table.insert(relation[rel], word)
        else
          relation[rel] = { word }
        end
      end

      -- create the pointers with relationships
      -- relation=table<word, string[]>
      local words = {}
      local pointers = {}
      for rel, dwords in pairs(relation) do
        if rel == '' then
          words = dwords
        else
          table.insert(pointers, {
            cpos = pos, -- inherit synset pos
            relation = rel,
            srcnr = 0, -- mythes has no specific relationships
            dstnr = 0, -- just general ones
            gloss = {},
            words = dwords,
          })
        end
      end

      local synset = {
        cpos = pos,
        words = words,
        gloss = {},
        pointers = pointers,
      }

      syns[#syns + 1] = synset
    end
  else
    return {}, '[error] unexpected line at offset: ' .. line
  end

  return { word = term, synsets = syns }, nil
end

--- searches Mythes thesaurus `word`, returns an item with 5 fields: term, syns, text, word, words
--- if table.term is nil, nothing was found. if err is also nil, nothing went wrong
---@param word string word for searching the thesaurus
---@return table|nil item { term = word_found, syns = { {(pos), syn1, syn2,..}, ..} } or nil if not found
---@return string|nil error message or nil
function M.search(word)
  assert(M.open())
  local line, item, err

  -- search idx for `word` to get offset to entry line in dat-file
  line, _, err = binsearch(M.fh.idx, word, '^[^|]+')
  if line == nil or err then
    return nil, err
  end

  -- pickup offset into dat file
  -- idx-line is <word>|<offset>
  local offset = tonumber(line:match('|%s*(%d+)$'))
  if offset == nil then
    return nil, '[error] dta offset not found on idx line'
  end

  -- read entry in dat file
  item, err = M.data(offset) -- item has term, syns fields

  assert(M.close())
  return item, err
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
---@return Item item thesaurus Item for given `word`
function W.search(word)
  W.open()
  word = word:gsub(' ', '_'):lower() -- ensure lowercase collocation
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

  return setmetatable(item, mt)
end

--[[ PICKER ]]

local ns = vim.api.nvim_create_namespace('ns_thesaurus')
local hl = {
  text = 'Special',
  word = 'Special',
  number = 'Number',
  pos = 'Comment',
  relation = 'Constant',
  trivial = 'Comment',
  pointer = 'Keyword',
}

function P.format(item, _)
  assert(item and item.word and item.synsets, 'malformed item:' .. vim.inspect(item))
  return {
    { ('%-25s | '):format(item.word):gsub('_', ' '), hl.word },
    { ('%s meanings'):format(#item.synsets), hl.trivial },
  }
end

function P.finder(opts, _)
  vim.print(vim.inspect({ 'finder', opts }))
  local item, err = W.search(opts.search)

  if err then
    vim.notify('[error] ' .. err, vim.log.levels.ERROR)
    return {}
  elseif item == nil then
    vim.notify('[warn] nothing found for ' .. opts.search, vim.log.levels.INFO)
    return {}
  end

  -- add additional related items
  local items = { item }
  local seen = { [item.word] = true }
  for _, synset in ipairs(item.synsets) do
    for _, word in ipairs(synset.words) do
      if seen[word] == nil then
        -- ignore errors, not found means nil means noop
        items[#items + 1] = W.search(word)
        seen[word] = true
      end
    end
  end

  -- snacks requires a field `text` for its matcher
  local set_text = function(itm)
    itm.text = itm.word
    return itm
  end

  return vim.tbl_map(set_text, items)
end

function P.preview(picker)
  local item = picker.item
  local m = function(lines, text, mark)
    -- add marked text to last line in lines
    if #lines == 0 then
      lines[1] = ''
    end
    local last = #lines

    if mark and #text > 0 then
      local col = #lines[last]
      local len = #text
      item.marks[#item.marks + 1] = { last - 1, col, col + len, mark }
    end
    lines[last] = lines[last] .. text
  end

  if item.lines == nil then
    item.marks = {}
    item.title = item.word:gsub('_', ' ') -- in case word is a collocation
    item.ft = 'markdown'
    local lines = {}
    local ix = 1
    for _, syn in pairs(item.synsets) do
      -- add synset words
      local pos = W.cpos_to_str[syn.cpos] or syn.pos
      m(lines, ('%d. '):format(ix), hl.number)
      m(lines, ('%s: '):format(pos), hl.pos)
      m(lines, ('%s'):format(table.concat(syn.words, ', ')):gsub('_', ' '), hl.word)

      for _, gloss in ipairs(syn.gloss) do
        lines[#lines + 1] = '- ' .. gloss
      end
      lines[#lines + 1] = ''
      ix = ix + 1

      for _, ptr in ipairs(syn.pointers) do
        -- local sword = (ptr.sword and ('%s'):format(ptr.sword) or ''):gsub('_', ' ')
        -- turgid
        local sword = ptr.srcnr == 0 and syn.words[1] or syn.words[ptr.srcnr]
        sword = sword:gsub('_', ' ')
        local dword = ptr.dstnr == 0 and table.concat(ptr.words, ', ') or ptr.words[ptr.dstnr]
        local ppos = W.cpos_to_str[ptr.cpos]
        lines[#lines + 1] = ''
        m(lines, ptr.relation, hl.relation)
        m(lines, ', ')
        m(lines, ppos .. ':', hl.pos)
        m(lines, ' ')
        if #sword > 0 then
          m(lines, sword, hl.word)
          m(lines, ' - ')
        end
        m(lines, (dword):gsub('_', ' '), hl.ptr_word)

        for _, gloss in ipairs(ptr.gloss) do
          lines[#lines + 1] = '+ ' .. gloss
        end
        lines[#lines + 1] = ''
      end
      lines[#lines + 1] = ''
    end
    item.lines = lines
  end

  -- update preview window
  picker.preview:set_lines(item.lines)
  picker.preview:set_title(item.title)
  picker.preview:highlight({ ft = item.ft })
  vim.api.nvim_set_option_value('wrap', true, { win = picker.preview.win.win })

  -- apply extmarks to preview buffer
  local buf = picker.preview.win.buf
  vim.api.nvim_buf_clear_namespace(buf, ns, 0, -1)
  for _, mark in ipairs(item.marks) do
    local row, col, end_col, hl_group = unpack(mark)
    vim.api.nvim_buf_set_extmark(buf, ns, row, col, { end_col = end_col, hl_group = hl_group })
  end
end

function P.confirm(args)
  -- default action for <enter>, unless that's been overridden
  vim.print(vim.inspect(args))
end

--[[ SYNS MODULE ]]

---simple select from mythes or wordnet thesaurus
---@param word string thesaurus search word
---@param mythes? boolean if true searches mythes, wordnet otherwise
function S.select(word, mythes)
  local item = mythes and M.search(word) or W.search(word)

  if item then
    local thesaurus = mythes and 'Mythes' or 'Wordnet'
    setmetatable(item, mt)

    local choices = {}
    item:words(function(dword, pos, gloss, sword, relation)
      table.insert(choices, { dword, pos, relation, sword, gloss[1] or '' })
    end)

    vim.ui.select(choices, {
      prompt = thesaurus .. ': ' .. word,
      format_item = function(c)
        local dword, pos, relation, sword, gloss = unpack(c) -- ignore gloss
        gloss = #gloss > 0 and ' - ' .. gloss or gloss
        local itm = '%-25s | %-15s | %s (%s) %s'
        return itm:format(dword, pos, sword or '!sword', relation or '!rel', gloss)
      end,
    }, function(choice, idx)
      vim.print('you choose ' .. (idx or 0) .. ': ' .. (vim.inspect(choice)))
    end)
  end
end

function S.thesaurus(word, opts)
  if word == nil or type(word) == 'table' then
    opts = word
    word = vim.fn.expand('<cword>')
  end
  opts = opts or {}

  local actions = {
    alt_enter = function(picker, item)
      -- (new) thesaurus search using current item or search input text
      local w = (item and item.word or picker.matcher.pattern):gsub(' ', '_')

      picker.input:set('', w) -- input search '', input prompt to word>
      picker.opts.search = w
      picker:find({ refresh = true })
    end,

    enter = function(picker, item)
      -- replace <cword> with item picked
      picker:close()
      if item and item.word then
        -- ("_) puts inner word in blackhole register `:h quote_`
        vim.cmd('normal! "_ciw<esc>' .. item.word:gsub('_', ' '))
      else
        vim.notify('no replacement selected')
      end
    end,
  }

  local win = {
    -- part of picker's options, snack's win config:
    -- linking keystrokes to action handler functions by name
    input = {
      keys = {
        ['<M-CR>'] = { 'alt_enter', mode = { 'n', 'i' } },
        ['<CR>'] = { 'enter', mode = { 'n', 'i' } },
      },
    },
  }

  local providers = {
    default = W,
    mythes = M,
    wordnet = W,
  }

  local p = providers[(opts.source or 'default'):lower()]
  if p == nil then
    vim.notify('No such thesaurus: ' .. opts.sources)
    return
  end

  local picker_opts = {
    title = 'thesaurus ' .. p.name,
    search = word:lower():gsub(' ', '_'),
    preview = P.preview,
    format = P.format,
    finder = P.finder,
    transform = P.transform,
    win = win,
    actions = actions,
    confirm = P.confirm,
    float = true,
  }
  return require 'snacks'.picker(picker_opts)
end

function S.test(word, mythes)
  local items = P.finder({ search = 'turgid' })
  vim.print(vim.inspect(items))
end

return S
