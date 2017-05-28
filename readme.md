naperian
===

[![Build Status](https://travis-ci.org/tonyday567/naperian.png)](https://travis-ci.org/tonyday567/naperian)

See https://tonyday567.github.io/naperian/index.html for project description.

~~~
stack build --test --exec "$(stack path --local-bin)/pandoc -f markdown+lhs -i src/Tensor.lhs -t markdown -o other/Tensor.md --filter pandoc-include --mathjax" --exec "$(stack path --local-bin)/pandoc -f markdown+lhs -i src/Slicer.lhs -t markdown -o other/Slicer.md --filter pandoc-include --mathjax" --exec "$(stack path --local-bin)/pandoc -f markdown+lhs -i other/blog.md -t html -o index.html --filter pandoc-include --mathjax" --file-watch
~~~
