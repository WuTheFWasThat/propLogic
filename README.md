## installation

```
cabal sandbox init
cabal install

cabal build
cabal run
# or
cabal repl propositionalLogic.hs
```

## TODO
- make function to parse sequent from output of show:

  ```
  parseSequent :: String -> Sequent
  parseSequent "prop , prop2 , prop3 |- prop4 , prop5 , prop6"
  ```

- separate file into separate modules

- put in a license (i'm okay with whatever)
