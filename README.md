# rhai-tpl

Customizable template engine that uses [`rhai`](https://rhai.rs) for its logic.

## Templating

Use `<% %>` for running logic you don't want to write to the template and use `<%= %>` for writing.

```
<%
let a = [42, 123, 999, 0, true, "hello", "world!", 987.6543];

// Loop through the array
for (item, count) in a { %>
Item #<%= count + 1 %> = <%= item %>
<% } %>
```

Returns:

```
Item #1 = 42
Item #2 = 123
Item #3 = 999
Item #4 = 0
Item #5 = true
Item #6 = hello
Item #7 = world!
Item #8 = 987.6543
```

## Rendering

The `rhai_tpl::Engine` enables rendering and customization. It has 2 generic params: `W: Write` and `S: State` which is state you can use when modifying the engine. it is implemented by default for any type implementing `Clone + Send + Sync + 'static`.

```rust
let f = std::fs::OpenOptions::new()
    .create(true)
    .write(true)
    .open(&filepath)?;

let engine = rhai_tpl::Engine::new::<std::fs::File, ()>();

let input = ""; // you'd fetch this from a file or whatever

let tpl = engine.compile(input)?;

tpl.render(f, ())?;
```

## Customizing

You can mutate the engine directly by adding / modifying functions available to users:

```rust
let mut engine = Engine::new::<std::fs::File, ()>();

engine.register_fn(
    "write",
    |tw: &mut TemplateWriter<std::fs::File, ()>,
        d: Dynamic|
        -> Result<(), Box<EvalAltResult>> {
        tw.write_all(format!("overloaded: {d}").as_bytes())
            .map_err(|e| Box::new(EvalAltResult::from(e.to_string())))?;
        Ok(())
    },
);
```

This modification overrides the default `write` output for the `rhai::Dynamic` type and prefixes it with `overloaded: `. It's not particularly useful, but it gives you an idea.