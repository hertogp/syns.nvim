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
