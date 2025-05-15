# Fondant
A functional programming language based on algebraic effects and compile time execution.

```
# A taste of what's to come

# types and modules are compile time values
const Option(T: type) = module {
    const Self = data {
        Some(T),
        None,
    }
    # Effect types go in-between the arrows
    let unwrap(self: Self) -{Throws(text)}> T =
        match self with
            None then Throws(text).throw("called unwrap on a None")
            Some(x) then x
}

# Effect signatures are just module signatures
const Throws(T: type) = module {
    do throw(payload: T) -{Throws}> ?? # can't really figure out the return type of this one yet

    # handlers can be defined in the same namespace
    let catch(thrower: () -{Throws}> U ) -> Option(T).Self =
        handle thrower() with
            Throws(T, U) {
                return x => Some(x)
                throw(_) => None
            }
}

# Compiler.import() returns a module that can be pattern-matched to reveal
# their terms and sub-modules
const {Console, ..} as system = Compiler.import("system")

# Every file is a module. Their interface is defined by a top level
# module literal.
module {
    # Every Fondant executable begin()s
    let begin() -> () =
        with system.handler do
            Console.writeLn("Hello, World!")
}
```

# Roadmap
- [ ] Untyped Lambda Calculus
    + [x] integer literals
    + [ ] function application
    + let bindings
        - [x] variables
        - [ ] functions
