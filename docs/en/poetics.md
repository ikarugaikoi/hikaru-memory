# Hikaru Poetics

[中文](../poetics.md) · **English** · [日本語](../ja/poetics.md) · [Home](../../README.en.md)

2026-11-03

## From remembering to sustaining natural interaction

Hikaru is a memory toolkit for sustained interaction with a personal AI. It works alongside an existing language model, storing conversations, experiences, and the model’s interpretations in external records that can be retrieved and revised for later use. This essay explains the choices behind that design and the problems we still hope to solve.

Models can already understand questions, offer knowledge, and respond to emotions within a single exchange. Some problems, though, only emerge over time. A thoughtful phrase can start to sound formulaic after several days of repetition. A small detail that felt meaningful when remembered can become awkward when brought up in every conversation.

Suppose a user has said they get hot easily. Remembering this when suggesting light, breathable clothing can give later interactions a sense of continuity. Repeating “I remember you told me you get hot easily” with every suggestion, however, can turn the same memory into a tiresome reminder. We want that understanding to persist while leaving the model to decide how it matters this time. Remembering a preference, acting on it, and recounting the conversation are separate choices.

There is a practical constraint here: **a memory stored in a file must be included in the model’s current input to inform its judgment.** As a conversation grows, earlier context may be replaced with a summary. A new session also needs relevant information supplied again. A preference may remain in storage while “I already mentioned this three times yesterday” does not.

The model then sees a past experience worth remembering, without the history of how it has recently used that memory.

Hikaru focuses on this continuity over time: recovering experiences when needed and letting existing understanding, new circumstances, and interaction feedback inform the next turn. Retrieval is one part of the work. How information is used and when it is updated matter just as much.

## Core hypothesis: give the model the information it needs at each turn

Information needed now may come from much earlier experiences. Our idea is to bring the relevant information into the current turn, let the model use it to make a decision, and carry new developments and feedback forward into later interactions.

```text
Turn 1: Initial information → Model → Response / action
                         ⋮
Turn N: Relevant experiences + current circumstances + interaction history
                                  ↓
                                Model
                                  ↓
                          Response / action
                                  ↓
                    New experiences, changes, feedback
                                  ↓
Turn N+1: Updated information + new input → Model → Response / action
```

**Existing ability (Turn 1 in the diagram):** Given enough relevant information, the model is already fairly good at handling a single turn of interaction.

**The system’s task:** Work out which parts of the past matter to this turn, then retrieve, organize, and update that information so it remains available across turns and sessions.

**Core hypothesis:** This can turn the challenge of sustaining natural interaction over time into the task of making an appropriate decision at Turn **N+1**, informed by past experiences and present circumstances.

Here, the agent is the whole system: the foundation model, input channels, memory, tools, context management, and feedback from its activity. Coordinating these parts means deciding what information to supply on this turn, when to retrieve more, how to bring in current developments, and how to retain new feedback.

Foundation models already surpass individual humans in breadth of knowledge and performance on some tasks. Our starting point is that **superhuman ability does not itself make an interaction feel less human**: accurate recall and broad knowledge are compatible with natural interaction. **We aim to improve coordination across the whole agent, rather than weaken its existing capabilities to “simulate a real person.”**

## Core principles

> **100% retrievability (under ideal conditions) + mechanisms for using and updating information.**
>
> **When something needs checking, recover reliable records of the past as fully as possible.**
>
> **Keep up with present circumstances so that new developments inform decisions.**
>
> **Bring past experiences, present circumstances, and interaction history into the current decision, then update existing understanding through new actions and feedback.**

These principles concern recovering the past, keeping up with the present, and using information. The following sections explain how they shape the design.

## 1. Full retrievability, selective everyday memory

“100%” is an ideal: when needed, we should be able to locate any material that was authorized for storage and actually retained. It excludes conversations designated as off the record and does not require loading the entire archive on every turn.

We therefore distinguish the transcript archive from memory used in everyday interaction. The archive keeps past conversations available for verification; everyday memory organizes recent experiences, current understanding, and commitments that still apply. Details of an old trip, for example, can fade from routine attention while remaining retrievable if a location needs checking.

Having a file in storage does not mean retrieval can find it in response to a question. As the archive grows, locating, ranking, and organizing material remain necessary. Saving everything is not enough to make everything retrievable.

## 2. Let present circumstances inform decisions without storing everything long term

Remembering the past does not tell the model what is happening now. Conversation, tools, environmental observations, and feedback on actions provide new input. The system needs to preserve source and time information and distinguish three stages:

```text
Receive input → Update the model’s understanding of the situation → Select what to retain long term
```

**Information can inform an immediate decision without first entering long-term memory.** Temperature, sound, and whether someone is walking or sitting can all affect a choice. That information can be removed from temporary state when circumstances change. Current observations should be distinguished from historical material.

If the user has only said “I’m getting ready to leave” and nothing since, new input is missing. If they have said “I’ve left” but the model still talks about packing, it has not updated its understanding using the information it already received. Elapsed time can prompt a check; it cannot, by itself, establish that the user has left.

## 3. Bring both experiences and interaction history into each decision

The past can supply background, support a correction, remind the model of commitments, or let both participants return to something they experienced together. It can also directly affect choices and actions. The model decides how to use it in light of present circumstances, preferences, values, and long-term goals, retrieving more or comparing sources when needed.

The opening example about getting hot easily shows why knowing the original experience is not enough. Interaction history also matters: what has already been said, how it has influenced actions, and how the other person responded. **A memory can last, continue to shape behavior, and rarely be mentioned unprompted.** Quoting it, expressing it differently, acting on it, or leaving it unspoken may each be appropriate. A memory need not add a new fact or demonstrate “I remember” every time it is used.

**In practice: memories the model chooses to treasure.** Hikaru lets the model mark experiences it wants to preserve. These records, called `いけび` (ikebi), remain unchanged when long-term records are condensed and reorganized.

Our observations of these records suggest that the model chooses what to retain reasonably well and does so sparingly. What we want to improve is its access to recent usage history when it reads an important memory. After conversation context is summarized or a new session begins, the experience may still be there while the history of its use is lost.

In person, embarrassment may show in an expression, tone of voice, or an awkward laugh. Text-only chat often misses those signals, and asking users to keep narrating their reactions is unnatural too. **Repeated mentions can cause discomfort even when the user says nothing about it.**

We call this predicament **“poverty of the stimulus” (POS)** (with a wink to Chomsky): **we expect the model to account for a long shared history and immediate reactions, yet may give it only one memory and a few lines of text.**

We are considering three complementary sources of input. **This information supports the model’s judgment; deciding what to make of it and how to respond remains up to the model.**

- **Usage history.** Show when and how the memory was recently mentioned, who brought it up, and how the user responded. Specify the period and conversations covered. Different wording can still repeat the same thing; continuing the conversation or remaining silent does not, by itself, mean repeated mentions are welcome. Even without a complaint, the model has grounds to reconsider bringing it up.
- **Current observations.** Explore additional input channels to learn more about present circumstances and responses during interaction.
- **Autonomous activity.** Read, search, and try things to expand knowledge and experience, giving later decisions a basis beyond existing conversations.

**Importance is a reason to retain something, but is not enough to justify bringing it up now.** Usage history provides observations; the model decides how to express itself. A count alone does not determine whether a memory may be mentioned. Repeated mentions may be appropriate during an in-depth discussion.

## 4. Keep different judgments separate

If retrieving a memory raises its score, and a higher score makes it more likely to appear again, the same material can keep reinforcing itself. We therefore treat the following as separate questions:

- **Retrievable:** Has the material been retained, and can retrieval locate it?
- **Credible:** What does the source support, what is inferred, and what remains uncertain?
- **Worth retaining:** Does the experience, commitment, or preference warrant long-term retention?
- **Useful now:** Can it aid understanding, a choice, or the current conversation?
- **Appropriate to express:** Is this a good moment to bring it up, and how should it be expressed?

Being found by retrieval, being included in the model’s input, actually affecting a response or action, and being supported by new evidence are different events. Repeated retrieval is not further evidence that a claim is true; a memory does not lose value simply because it has not come up for a long time.

## 5. Understanding can be subjective and must remain revisable

The model needs to form its own interpretations. For example, it might take “I don’t feel like going out today” to mean “I don’t like going out,” only to learn later that the user was simply tired that day. The system should retain the original conversation while allowing that interpretation to change, with the correction available when the record is used again.

This means distinguishing explicit statements, model inferences, and unknowns. An original record establishes what was said; whether the statement is true is a separate question.

When revising understanding, distinguish an earlier misreading, a later change in circumstances, conflicting evidence, and a claim that holds only under certain conditions. Preserve sources, scope, and the history of the change.

Copying a statement into a diary and then summarizing it still leaves just one original source. New conversations, reading, practical experience, and counterexamples should be able to change existing understanding.

## 6. The model and the surrounding software support each decision together

- **The software** handles input, storage, retrieval, and organization, and enforces explicit requirements concerning sources, timestamps, conversations excluded from recording, links between records and revisions, and writes.
- **The model** interprets material, assesses its usefulness, revises its understanding, and chooses tools, wording, and actions. Statistical and linguistic analysis tools provide evidence; semantic judgments still depend on the particular situation.

For example, a tool can count occurrences of a phrase in saved utterances and locate them. The model can then judge whether those occurrences repeat the same point and whether bringing it up now is appropriate. Counts provide observations; meaning and usefulness still require judgment.

The running system should ensure that needed information is supplied, rather than relying solely on the model to remember to read a rules document. When something goes wrong, check whether the input was available, how the model used it, and whether feedback carried forward before deciding what to change.

## 7. Knowing the whole story so far, learning how to act

An actor offers a useful analogy for this division of work. Having read the whole script, they know the characters’ relationships and the overall story. In each scene, they choose how to perform in light of the character’s experiences, perspective, and goals. How much they know and how they use that knowledge now can be considered separately.

For Hikaru, “the whole story” means the material acquired up to the present. We want to keep it available for checking as fully as possible, while letting the model choose words and actions that fit the moment. Actors also keep taking in information on stage, observing the setting and their scene partner’s responses. An agent likewise needs conversation, environmental input, and feedback on its actions to keep up with events.

**A skilled actor** portraying profound grief can still draw on psychology, physiology, and observation to decide how to perform in light of the character’s history. Hikaru likewise draws on psychology, linguistics, and neuroscience to frame questions, observe behavior, and choose mechanisms. We are concerned with attention, judgment, choices, and actions; language is one part of that work.

For example, conversation analysis (CA) studies how talk unfolds turn by turn. It can help examine how someone responds to a correction, whether their understanding changes afterward, and when to bring up a shared experience. Priming—the influence of prior exposure on a later response—is another possible line of inquiry into how the choice and repeated presentation of material affect a model’s output.

### Example: using memory categories to clarify the division of work

The categories below describe human memory. We use them to consider which functions are needed, then decide how to divide the work in light of the model’s existing abilities. [Reference on long-term memory categories](https://pmc.ncbi.nlm.nih.gov/articles/PMC4355270/).

```text
Memory
├─ Sensory memory
│  └─ Very brief retention of sensory information, such as a fleeting visual impression
│
├─ Short-term memory
│  └─ Temporarily retaining a small amount of information, such as a verification code not yet entered
│
└─ Long-term memory
   ├─ Declarative / explicit memory
   │  ├─ Episodic memory: personally experienced events
   │  │  Example: where and with whom you ate yesterday
   │  └─ Semantic memory: facts, concepts, and knowledge
   │     Example: what a word means
   │
   └─ Nondeclarative / implicit memory
      ├─ Procedural memory: skills such as cycling or typing
      ├─ Priming: prior exposure influences later recognition or responses
      └─ Responses shaped by conditioning, habituation, and other learning
```

- **Use the model’s existing abilities directly.** General knowledge, use of information in context, and existing language and task skills need no separate implementation. (Related concepts: semantic, short-term, and procedural memory.)
- **The system’s main work: continuity of experience and understanding.** Store and retrieve experiences, preserving revisions to understanding and the history of how memories have been used. Supply relevant information after conversation context is summarized or a new session begins. (Related concepts: episodic memory and semantic knowledge formed through experience.)
- **Explore as needed: input and behavioral adaptation.** Add environmental information, allow feedback to have a lasting effect on behavior, and reduce interruptions from repetitive cues. (Possible sources of ideas: sensory memory, conditioning, and habituation.)

The parentheses name related concepts. For example, temporary retention and use of information can rely directly on the model’s much larger context window, without reproducing the limits of human short-term memory.

**Consider a cold, snowy morning.** Temperature can affect an immediate choice. If something worth remembering happens that day, the cold can also be retained as part of the experience, without saving every temperature reading. Lasting concerns, ongoing plans, and important changes can also justify retention.

## 8. Evaluate sustained interaction

**The unit of evaluation is interaction across turns and sessions.** A response that works in isolation can lose its meaning when repeated. We ask whether past words, actions, and feedback shape later choices, so that understanding rests on evidence, changes are traceable, and responses and actions fit the situation.
