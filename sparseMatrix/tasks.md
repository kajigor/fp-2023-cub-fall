
# Very easy tasks (you can earn no more than 6 points in total)

1. [1 point] Give an example of closed term in normal formal which has type $(\gamma  \to  \epsilon) \to ((\gamma  \to  \epsilon) \to  \epsilon) \to  \epsilon$, but which cannot be typed with the type $\alpha  \to (\alpha  \to  \epsilon) \to  \epsilon$.

$\lambda f.\lambda g. g(\lambda x.f x)$

2. [1 point] Give examples of types having the following powers:

1) $|a| + |b| * |c| + |b|^2 * |a|$

`T a b c = A a | B b c | C b b a`

2) $|a|^0$

`Void -> a`

3) $2 + |a|^{(|b|^{|c|})} * |c|$

`T a b c = A Bool | C ((c -> b) -> a) c`

4) $|a|^{(|a|*|b|)} + |b|^{|c|} + c^0 + b^0$

`T a b c = A ((a, b) -> a) | B (c -> b) | C (Void -> c) | D (Void -> d)`

5) $|a|^{(|b|+|c|)} + 2 + |a|^0$

`T a b c = A ((b, c) -> a) | B Bool | C (Void -> a)`

6) $|a|^{(|b|*|c|)} + |c|^0$

`T a b c = A ((b, c) -> a) | C (Void -> c)`

3. [2 points] Find closed terms in normal form that inhabit type (if any) $(\delta  \to  \delta  \to  \alpha) \to (\gamma  \to  \alpha) \to (\alpha  \to  \beta) \to  \delta  \to  \gamma  \to  \beta$ (2 terms; formally prove that they really have the given type)

1) $(\delta  \to  \delta  \to  \alpha) \to (\gamma  \to  \alpha) \to (\alpha  \to  \beta) \to  \delta  \to  \gamma  \to  \beta$

2) $(\alpha\to(\beta\to\gamma))\to((\alpha\to(\gamma\to\delta))\to(\alpha\to(\beta\to\delta)))$

3) $((((\alpha\to\beta)\to\alpha)\to\alpha)\to\beta)\to\beta$

4) $Void \to Void$

4. [1 point] Give an examples of closed terms in pure lambda calculus:

1) Term that is strongly normalizing

$\lambda x.\lambda y.(\lambda w.wy)((\lambda z. zy)x)$

2) Term that is weakly normalizing but not strongly normalizing
$\lambda x.\lambda y.x(\lambda x.xx)(\lambda x.xx)$
3) Typable in system F but not in HM
$\lambda x.(x  0,x (true))$
5. [1 point] Using standat encoding of booleans in lambda calculus evaluate $If\ False\ \Omega\ \lambda x. x$ under call-by-name semantics. What happens when you evaluated it under call-by-value semantics?

6. [2 point] Assume we have a lambda calculus with integers, booleans, conditionals, etc. Consider the following higher-order function $H$.

$H \equiv  \lambda f. \lambda n. if\ n = 1\ then\ true\ else\ if\ n = 0\ then\ false\ else\ not\ (f (n − 1))$

1. Suppose that $g$ is the fixed point of $H$. What does $g$ compute?

2. Compute $Y\ H$ under call-by-name semantics. What has happened to the function call $f (n − 1)$?

3. Compute $(Y\ H)\ 2$ under call-by-name semantics.

  

# Other tasks

  

1. [4 points] Write an efficient program that finds the smallest natural number missing in the given set. What is the complexity of our solution?

2. [4 points] Implement real-time dequeue

3. [5 points] Extend STLC with `Unit` being one of the base types. Is there a way of constructing a sequence of terms $t_1 , t_2 , ..., t_n, ...$ in the simply typed lambda-calculus with only the base type `Unit`, such that, for each $n$, the term $t_n$ has size at most $O(n)$ but requires at least $O(2^n)$ steps of evaluation to reach a normal form?

4. [6 points] Implement a type-safe API for for weights (kilograms, pound, pood). Requirements:

* Three functions that convert a float number to a weight in metric system, royal system, and poods.

* One function that adds

* One function for adding kilograms with kilograms, pounds with pounds and poods with poods.

* There should be no functions for converting weights to float.

* A function that will print out the weight in the corresponding dimension.

* Observe zero-cost abstraction: if one needs to add weights at runtime, then program should add and do nothing else.

5. [5 points] There is another way of computing the predecessor function on Church numerals. Let $k$ stand for the untyped lambda-term $\lambda x. \lambda y. x$ and $i$ for $\lambda x. x$. The untyped lambda-term $vpred = \lambda n. \lambda s. \lambda z. n (\lambda p. \lambda q. q (p s)) (k z) i$ computes the predecessor of an untyped Church numeral. Show that this term can be typed in System F by adding type abstractions and applications as necessary and annotating the bound variables in the untyped term with appropriate types.Explain why it works!

$\lambda n^{\Lambda \alpha \to (\alpha \to \alpha) \to \alpha \to\alpha}\Lambda \alpha.\lambda s^{\alpha \to \alpha}\lambda z^{\alpha}. (n^{(\alpha\to\alpha)\to\alpha}(\lambda p^{(\alpha\to\alpha)\to\alpha}.\lambda q^{\alpha\to\alpha}.q(ps))(k^{\alpha, (\alpha\to\alpha)} z))(i^{\alpha\to\alpha})$

It works by substituing `k` in place of one of the succesor functions, thus neutralizing it.
6. [6 points] In pure System F (without fix), write a function insert of type $\forall X. (X \to X \to Bool) \to List X \to X \to List X$ that takes a comparison function, a sorted list, and a new element, and inserts the element into the list at the appropriate point (i.e., after all the elements smaller than it). Next, use insert to build a sorting function for lists in pure System F.

7. [6 points] Prove (in STLC) the permutation lemma : let $\Gamma  \vdash t : T$ and $\Delta$ be a permutation of $\Gamma$, then $\Delta  \vdash t : T$. Moreover, proove trees has the same size.

8. [4 points] Extend STLC with `Unit` being one of the base types; equip the calculus with an operator of sequential execution ";" (adding the appropriate evaluation and typing rules)

9. [4 points] Extend STLC with "dummy abstractions (wildcards)", i.e. abstractions $\lambda  \_ :\tau . t \equiv  \lambda x:\tau . t, x \not\in FV(t)$. Provide the appropriate typing and evaluation rules for it, choosing an arbitrary generally accepted computational strategy.

10. [6 points] Prove (in STLC with `Bool` being one of the base types) Lemma [Canonical Forms]: If `v` is a value of type `Bool`, then `v` is either `True` or `False`.

11. [5 points] Prove the weakening lemma: if $\Gamma  \vdash t : T$ and $x \not\in Dom(\Gamma)$, then $\Gamma, x:S \vdash t : T$. Moreover, prove that they have derevation trees of the same size (height)