# purescript-pagination

An attempt to make a more type-safe paginator using PureScript idioms.

Status: design seems ok, but it hasn't been tested too much yet and the lawfulness of the typeclasses haven't been checked.

To do:
- Check lawfulness of typeclass instances.
- Add a real-world example. For example, tupling a pager with a list of records, like `type PagedUsers = Tuple Pager (Array User)`
- Add instances for other type-level number implementations?
- Move SimpleNat to different repo, or remove completely in favor of better typelevel naturals.

## Examples

See [Test.Main](test/Main.purs) module in the `test` directory

## License

[MIT License](LICENSE)
