[![Build Status](https://travis-ci.org/pheymann/specdris.svg?branch=master)](https://travis-ci.org/pheymann/specdris)

# specdris
With this framework you can write spec-like **Unit Tests** in Idris:

```Idris
import Specdris.Spec

main : IO ()
main = spec $ do
  describe "This is my math test" $ do
    it "adds two natural numbers" $ do
      (1 + 1) `shouldBe` 2
    it "multiplies two natural numbers" $ do
      (2 * 2) `shouldBe` 3
    it "do fancy stuff with complex numbers" $ do
      pendingWith "do this later"
```
You can also nest `describe`. When executed this spec it will produce the following output:

```
This is my math test
  + adds two natural numbers
  + multiplies two natural numbers
    [x] 4 /= 3 -- red
  + do fancy stuff with complex numbers
    [] pending: do this later -- yellow
    
Failed: Total = 3, Failed = 1, Pending = 1 -- red
```

You can also test your `IO` code:

```Idris
import Specdris.SpecIO

main : IO ()
main = specIO $ do
  describe "This is my side effect test" $ do
    it "say my name" $ do
      name <- loadName
      
      pure $ name `shouldBe` "Foo"
```

You can find more information about `SpecIO` [here](#specio).

## Install
Clone the repo from github with `git clone https://github.com/pheymann/specdris.git` and run:

```
cd specdris
./project --install
```

## Documentation
### Expectations
Currently this framework provides you with:

|Expecation|Alias|Description|
|----------|-----|-----------|
|`a shouldBe b`|`===`|is `a` equal to `b`|
|`a shouldNotBe b`|`/==`|is `a` unequal to `b`|
|`a shouldBeTrue`| |is `a` `True`|
|`a shouldBeFalse` | | is `a` `False`|
|`a shouldSatisfy pred`| | satisfies `a` a given predicate|

### Failed Test Cases
If an expectations in a test case failes the following expectations aren't executed and the
whole case is marked as failure:

```Idris
  it "failing test" $ do
    1 `shouldBe` 1 -- success
    2 `shouldBe` 1 -- failes
    2 `shouldBe` 2 -- will not be executed
```

### SpecIO
Besides the usual test cases you can also add effects as:

#### BeforeAll
Executes an `IO ()` before running any test case:

```Idris
specIO {beforeAll = putStrLn "hello"} $ do ...
```

#### AfterAll
Executes an `IO ()` after all test cases are executed:

```Idris
specIO {afterAll = putStrLn "bye"} $ do ...
```

#### Around
Takes a function `IO SpecResult -> IO SpecResult` which can be used to execute `IO` code
before and after every test case:

```Idris
around : IO SpecResult -> SpecResult
around resultIO = do putStrLn "hello"
                     result <- resultIO
                     putStrLn "bye"
                     
                     pure result

specIO {around = around} $ do
```
