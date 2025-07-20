# Syns - search a thesaurus

Search a thesaurus for synonyms and antonyms.

## Wordnet

The [wordnet](https://wordnet.princeton.edu/citing-wordnet) lexical database
files are courtesy of the [Princeton
University](https://wordnet.princeton.edu/) from their WordNet (TM) project.
`Syns` is using the version [3.1](https://wordnetcode.princeton.edu/wn3.1.dict.tar.gz)
database files.

These include:

| #  | index       | data       |
|:--:|:----------- |:---------- |
| 1  | index.noun  | data.noun  |
| 2  | index.verb  | data.verb  |
| 3  | index.adj   | data.adj   |
| 4  | index.adv   | data.adv   |
| -- | ----------- | ---------- |

It also has:
- `<pos>.exc` morphology exception lists (4)
- `sentidx.vrb` for easier searching in the index
- `sents.vrb` example sentences for verbs
which are not use by this plugin.

As explained on their [website](https://wordnet.princeton.edu/), wordnet is
more that a thesaurus. It not only groups words based on their meanings, but
also links to specific senses of words as well as labelling semantic relations
among words.

This plugin only uses the `index.<pos>` and `data.<pos>` files for simplicity.


### Structure of index files

see `:Open https://wordnet.princeton.edu/documentation/wndb5wn`

Structure:
```
lemma pos synset_cnt p_cnt [symbol ..] sense_cnt tagsense_cnt [synset_offset ..]
```
where:
- fields are separated by whitespace
- [..] may or may not be present
- `lemma`, lower case ascii word or collocation (with '_' connecting words)
- `pos`, part-of-speech symbol: `n` noun, `v` verb, `a` adjective and `r` adverb files
- `synset_cnt`, number of synsets for `lemma `in `pos`
- `p_cnt`, number of pointers that `lemma` has in synsets in `pos `containing `lemma`
- `[symbol..]`, space separated list of all different types of pointers that `lemma `has
- `sense_cnt`, same as `synset_cnt` kept for backwards compatibility
- `tagsense_cnt`, number of senses of `lemma `ranked according to frequency of use
- `[synset_offset ..]`, byte-offsets in data.<pos> file of synsets containing `lemma`

### Structure of data files

see `:Open https://wordnet.princeton.edu/documentation/wndb5wn`

Structure of a synset in data.<pos>:
```
offset lexofnr ss_type w_cnt [word lexid ..] p_cnt [ptr...] [frames...] | gloss
```
where:
- fields are separated by whitespace
- `|` separates `gloss `from the other fields
- [..] may or may not be present
- `offset`, 8 digit decimal nr, the current byte offset in this file
- `lexofnr`, 2 digit decimal nr, the file id of the lexographer file containing the synset
- `ss_type`, single character, part-of-speech symbol for this synset
   (`n` noun, `v` verb, `a` adjective, `s` adjective-satellite, `r` adverb)
- `w_cnt`
- `[word lexid ..]`
- `p_cnt`
- `[ptr ..]`
- `[frame ..]`
- `gloss`

Note: adjective-satellite synsets are located in `data.adj`

