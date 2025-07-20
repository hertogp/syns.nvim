# Syns - search a thesaurus

Search a thesaurus for synonyms and antonyms.

## Wordnet

The [wordnet](https://wordnet.princeton.edu/citing-wordnet) lexical database
files are courtesy of the [Princeton
University](https://wordnet.princeton.edu/) from their WordNet (TM) project.
`Syns` is using the version [3.1](https://wordnetcode.princeton.edu/wn3.1.dict.tar.gz)
database files.

These include:

-- -------------- -------------
1. index.noun     data.noun
2. index.verb     data.verb
3. index.adj      data.adj
4. index.adv      data.adv
-- -------------- -------------

It also has morphology exception lists:
1. noun.exc
2. verb.exc
3. adj.exc
4. adv.exc

and files used by search code to display sentences illustrating the use of some
specific verbs:

1. sentidx.vrb
2. sents.vrb

As explained on their [website](https://wordnet.princeton.edu/), wordnet is
more that a thesaurus. It not only groups words based on their meanings, but
also links to specific senses of words as well as labelling semantic relations
among words.

`Syns` only scratches that surface and uses on the index.<pos> and data.<pos>
files for simplicity.


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
- `tagsense_cnt`, number of senses of `lemma `ranked according to frequence of use
- `[synset_offset ..]`, byte-offset in in data.<pos> file of a synset containing `lemma`


