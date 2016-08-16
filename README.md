# syllogism
Syllogism solver with natural language input

## Description

A *syllogism* is a logical statement describing connections between individuals and categories, namely between a *subject* and a *predicate*. There are four main types of syllogistic statements:

- "Every S is P", abbreviated as SaP
- "No S is P", abbreviated as SeP
- "Some S is P", abbreviated as SiP
- "Some S is not P", abbreviated as SoP

Here, the lowercase letter between the subject and the predicate represents the *type* of the syllogism.

A syllogism can be inferred from two other syllogisms that contain either the subject or the predicate, called respectively the minor premise and the major premise, based on 24 different rules of inference, which are represented by the types of the three syllogisms and a figure (an integer between 1 and 4) that describes the arrangements of the subjects and predicates of each premise. 

Example:

> All men are mortal (major premise)

> Socrates is a man (minor premise)

> Therefore, Socrates is mortal (conclusion)

This is a special case of the AAA-1 (every statement is an A, and the figure is 1) rule of inference, referred to as B**a**rb**a**r**a**.
