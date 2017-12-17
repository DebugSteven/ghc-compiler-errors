Abstract Title: A Pragmatic Guide to GHC Errors
Abstract Topic: Concepts, Cautionary Tales
Submission Type: Educational Session (50m)

>Abstract Summary: When you’re just getting started with Haskell, you might write code that you would expect to work, but GHC complains and maybe it suggests a language extension. But what do those extensions really do? Using code examples, we are going to walk through what these language extensions mean, when you will need to use them, and when turning on language extensions would end up causing more problems than they would solve. We’ll dig into the history of Haskell including why particular extensions are on by default in GHCi and the implementation choices chosen in some typeclasses.

My initial thought here is that the abstract title doesn't match the
abstract summary. It said "pragmatic guide to GHC errors", not
"pragmatic guide to GHC asking for a language extension." The latter
is hard to make a talk out of because 80% of the time you should just
turn on the extension GHC wants, 20% of the time you made a mistake
that mislead GHC. I think it's worth covering type and compiler errors
that trigger GHC asking for a language extension, but not as a whole
talk. My suggestion from when we were walking the dogs stands, I think
a promising talk is one that covers increasingly complicated (at two
levels) GHC errors and tells you how to understand both the mistake
and how the error derives from the mistake. The "increasingly
complicated" is both ramping up the examples for a single type of
error and the kinds of errors you show them overall.

>Code examples will show errors, work arounds that don’t require the use of extensions in some cases, why you might choose to not use an extension and reasons behind that, and the original errored code that can be fixed with a language extension. Part of this presentation will be running code live so attendees can see what’s going on and so the problems are properly demonstrated.

There's not a lot being said here, it's still hooking back into
language extensions specifically which is not what I think people will
want to _exclusively_ hear about for 50 minutes. That's my value
judgment though.

>Content Relevancy: Haskell beginners attending this talk will be able to identify and fix GHC compiler errors more quickly after this talk by seeing common errors solved by language extensions. They’ll also get to learn the history and intention behind certain language choices which will lead to writing more thoughtful and error free code in the future.

Beginners will benefit most, but beginner-intermediate and
intermediates can benefit a lot from knowing the _meaning_ and
_origin_ of an error. An example is the _reason_ and _history_ of the
monomorphism restriction I gave you, but you may not necessarily want
to open that can of worms in much detail in a tak that intends to
cover good ground in terms of GHC errors.

Brain-dump people are asking from you is whatever. If it were me, I'd
broaden the scope from language extensions a bit, and then start
really digging into the praxis of the talk without necessarily
committing yourself to nitty gritty details.
