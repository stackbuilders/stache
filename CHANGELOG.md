## Stache 0.2.1

* Made TH parse errors nicer.

## Stache 0.2.0

* Breaking change: the `renderMustache` function will throw an exception
  when referenced key was not provided. This is a better behavior than
  silent interpolation of an empty string, because missing values are almost
  always a mistake and it's easy to provide empty strings explicitly anyway.

* Allowed `directory-1.3.0.0`.

## Stache 0.1.8

* Rename `specs` directory to `specification` as the previous name somehow
  caused conflicts when deploying an application on Heroku with Stache as a
  dependency.

## Stache 0.1.7

* Added `mustache` quasi-quoter.

## Stache 0.1.6

* Fixed a bug in the lookup algorithm for dot-separated keys.

## Stache 0.1.5

* When section's key is a number or non-empty string, it's accessible as `.`
  in the section body.

* Allow Aeson 1.0.

## Stache 0.1.4

* Numbers are now treated as “true” values.

* Added `Semigroup` instance for `Key`.

* Now change of delimiters affects special unescaped variable syntax as
  well, for example: `{{=<< >>=}}<<{var}>>` will be parsed as unescaped
  variable `var`.

* `compileMustacheFile` now shows full path to malformed template when the
  template cannot be parsed.

* Defined `displayException` method of `Exception` type class for
  `MustacheException` using `parseErrorPretty` from Megaparsec.

## Stache 0.1.3

* Cosmetic improvements.

* Minor improvement in performance of parser.

## Stache 0.1.2

* Fixed compilation of benchmarks with Megaparsec 5.0.1 and later.

## Stache 0.1.1

* Added benchmarks.

## Stache 0.1.0

* Initial release.
