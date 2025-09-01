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
