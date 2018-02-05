# purescript-pagination

An attempt to make a more type-safe paginator using PureScript idioms.
This is an experimental project, but it might still be useable for someone.

Status:
- Type-level paging: The design seems ok, but it hasn't been tested too much yet and the lawfulness of the typeclasses haven't been checked. This design feels like a heavy hammer (type-level checking of page count and sizes) for a relatively simple problem.
- Machine paging: The design feels better than type-level paging. This one feels heavy, but perhaps it's because it uses a StateT and ContT monad. It could be trimmed up a bit. The test case needs to be finished - I couldn't figure out how to run the transformers, `PagerMonad m -> m`.

My use-case is an in-browser app. I want the pagination state to be stored in the URL, which means the MealyPager is doing too much (it currently wants to hold that state). I'd welcome a PR to trim that off the MealyPager.

To do:
- Check lawfulness of typeclass instances.
- Add instances for other type-level number implementations?
- Move SimpleNat to different repo, or remove completely in favor of better typelevel naturals.
- Fix MealyPager test.
- Fix runPager for MealyPager, or remove that responsibility from the MealyPager.

## Examples

See [Test.Main](test/Main.purs) module in the `test` directory

## License

[MIT License](LICENSE)
