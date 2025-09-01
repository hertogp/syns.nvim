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

--[[ PICKER ]]
-- a snacks picker

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

  local snacks = require 'snacks'
  if not snacks then
    return S.select(word)
  end

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

function S.test(word)
  -- vim.fn.spellsuggest
  -- vim.fn.spellbadword -> { bad-word, bad|rare|local|caps|'' }
  local bad = vim.fn.spellbadword(word)
  local word = bad[1] == '' and word or bad[1]
  -- vim.fn.expand('<cword>') or bad[1]
  local suggestions = vim.fn.spellsuggest(word, 25, bad[2] == 'caps')
  vim.ui.select(suggestions, {
    prompt = 'Spelling : ' .. word,
  }, function(choice, idx)
    vim.print('you choose ' .. (idx or 0) .. ': ' .. (vim.inspect(choice)))
  end)
end

return S
