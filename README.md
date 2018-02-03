# purescript-pagination

An attempt to make a more type-safe paginator using PureScript idioms.
This is an experimental project, but it might still be useable for someone.

Status: The design seems ok, but it hasn't been tested too much yet and the lawfulness of the typeclasses haven't been checked. This design feels like a heavy hammer (type-level checking of page count and sizes) for a relatively simple problem.

I'd like to add another design to this repo which enables easy paging of server-side data. I feel like I need to store the paginator data structure in my browser app, but I can't store `Paged` data structure if the whole data structure isn't known at compile-time, as it's only available in an existential scope, which means it needs to be transformed to a different form before storing in my browser app's state. Maybe something like this: http://www.alfredodinapoli.com/posts/2016-09-10-paginators-are-mealy-machines-in-disguise.html

To do:
- Check lawfulness of typeclass instances.
- Add instances for other type-level number implementations?
- Move SimpleNat to different repo, or remove completely in favor of better typelevel naturals.

## Examples

See [Test.Main](test/Main.purs) module in the `test` directory

## License

[MIT License](LICENSE)
