# Wridentify: Identifying writing systems (languages) in texts

This program identifies the language of texts, with the specific aim
of efficiently identifying from the list of **over 2,000 languages**
maintained by the [*An Crubadan Project*](http://crubadan.org), of
which this program is a component.

Most NLP logic in the program lives in the (also under development)
companion library
[nlp-tools](https://github.com/RoboNickBot/nlp-tools), in the hopes
that it can be immediately useful in further projects.

At this early stage, using the ```wrident``` program requires you to
place the frequency lists and target text in pretty specific places;
if you want to try it out, read through ```src/Main.hs``` to figure
them out.

