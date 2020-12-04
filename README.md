# Almond

[![codecov](https://codecov.io/gh/lukechu10/almond/branch/main/graph/badge.svg?token=NFWU048HGY)](https://codecov.io/gh/lukechu10/almond)

A new JavaScript general purpose parser. Almond aims to generate ESTree compatible abstract syntax trees. Almond uses [nom](https://github.com/Geal/nom) for parsing.

This project currently is very work in progress. It can parse popular libraries like *jQuery*, *React*, *React-DOM*, *Backbone.js* etc... (Check out `benches/js/` directory).
Some obscure syntax might not parse correctly yet. If you find a bug, please consider reporting it via a GitHub Issue.

## Implementation Progress

- [x] ES5
- [ ] ES2015 (ES6)
- [x] ES2016 (ES7)
- [x] ES2017
- [ ] ES2018
- [ ] ES2019
- [ ] ES2020
