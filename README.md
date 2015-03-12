# nlp-tools: Utilities for Natural Language Processing

This is a collection of experimental programs for exploring NLP
tasks using data from the [*An Crúbadán* Project](http://crubadan.org),
of which this collection is a component.

Most NLP logic in these programs lives in the (also under development)
companion library
[nlp-libs](https://github.com/RoboNickBot/nlp-libs), in the hopes
that it can be immediately useful in further projects.

-----------------------------------------------------------------------

## Tools

### wrident: identifying the writing system (language) used in a text

This program identifies the language of texts, with the specific aim
of efficiently identifying from the list of **over 2,000 languages**
maintained by the [*An Crúbadán* Project](http://crubadan.org).

At this early stage, using the ```wrident``` program requires you to
place the frequency lists and target text in pretty specific places;
if you want to try it out, read through ```src/Main.hs``` to figure
them out.

