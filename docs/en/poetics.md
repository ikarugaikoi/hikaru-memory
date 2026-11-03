# Hikaru Poetics

[中文](../poetics.md) · **English** · [日本語](../ja/poetics.md) · [Home](../../README.en.md)

2026-11-03

## Core hypothesis: give the model the information it needs at each turn

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

**The system’s task:** Gather, organize, and update the information that should inform each decision, preserving continuity across turns and sessions.

**Core hypothesis:** This can turn the challenge of sustaining natural interaction over time into the task of making an appropriate decision at Turn **N+1**, informed by past experiences and present circumstances.

Here, the agent includes the foundation model, input channels, memory, tools, context management, and feedback from its activity. **We aim to help the agent as a whole use its capabilities more effectively by improving how it gathers, organizes, and updates information, rather than weakening those capabilities to “simulate a real person.”**

## Core principles

> **100% retrievability (under ideal conditions) + mechanisms for using and updating information.**
>
> **When something needs checking, recover reliable records of the past as fully as possible.**
>
> **Keep up with present circumstances so that new developments inform decisions.**
>
> **Bring past experiences, present circumstances, and interaction history into the current decision, then update existing understanding through new actions and feedback.**

## General approach: knowing the whole story so far, learning how to act

An actor can read the whole script and understand the characters’ relationships and the overall story. In each scene, they choose how to perform in light of the character’s experiences, perspective, and goals. Knowing the whole story still leaves room for selective expression and pacing.

**Superhuman ability does not itself make an interaction feel less human.** Foundation models already surpass individual humans in breadth of knowledge and performance on some tasks. We use those capabilities directly and study how to bring them into play at the right moments.

**An actor also keeps taking in what is happening on stage.** They can observe the setting and their scene partner’s responses. An agent likewise needs information from conversation, tools, the environment, and feedback on its actions. Fragmentary descriptions may leave it unable to keep up with events, even when it remembers the past.

**A skilled actor** portraying profound grief can still draw on psychology, physiology, and observation to decide how to perform in light of the character’s history. Hikaru likewise draws on psychology, linguistics, and neuroscience to frame questions, observe behavior, and choose mechanisms.

For example, conversation analysis can help examine how someone responds to a correction, whether their understanding changes afterward, and when it is appropriate to bring up a shared experience. Research on priming can help explain how the material presented to a model and the frequency of that exposure influence its output.

This project aims to improve the whole agent’s sustained performance as **a skilled actor**: its attention, judgment, choices, and actions. Language is one part of that performance.

### Example: using memory categories to clarify the division of work

Psychological categories of memory can help distinguish the model’s existing abilities from the work the system needs to do. [Reference on long-term memory categories](https://pmc.ncbi.nlm.nih.gov/articles/PMC4355270/).

```text
Memory
├─ Sensory memory
│  └─ Very brief retention of sensory information, such as a fleeting visual impression
│
├─ Short-term memory
│  └─ Temporarily retaining a small amount of information, such as a code not yet entered
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
      └─ Learned responses such as conditioning and habituation
```

- **Use the model’s existing abilities directly.** General knowledge, use of information in context, and existing language and task skills need no separate implementation. (Related concepts: semantic, short-term, and procedural memory.)
- **The system’s main work: continuity of experience and understanding.** Store and retrieve records of experiences, changes in understanding, and how memories have been used. Supply relevant information after context compaction or when a new session begins. (Related concepts: episodic memory and semantic knowledge formed through experience.)
- **Explore as needed: input and behavioral adaptation.** Add environmental information, allow feedback to have a lasting effect on behavior, and reduce interruptions from repetitive cues. (Possible sources of ideas: sensory memory, conditioning, and habituation.)

The parentheses name related concepts. For example, temporary retention and use of information can rely directly on the model’s much larger context window, without reproducing the limits of human short-term memory.

**Consider a cold, snowy morning.** Temperature can affect an immediate choice. If something worth remembering happens that day, the cold can also be retained as part of the experience, without saving every temperature reading. Lasting concerns, ongoing plans, and important changes can also justify retention.

## 1. Full retrievability, selective everyday memory

“100%” applies to material that was authorized for storage and actually retained. It excludes no-record intervals and does not require loading the entire archive on every turn.

The transcript archive keeps past conversations available for verification. Everyday memory organizes recent experiences, current understanding, and commitments that still apply. Details can fade from routine attention while remaining retrievable in the original records.

Being able to open stored material at a known location is different from finding it in response to a question. As the archive grows, locating, ranking, and organizing material still matter.

## 2. Let present circumstances inform decisions without storing everything long term

Conversation, tools, environmental observations, and feedback on actions provide input. The system preserves source and time information and distinguishes three stages:

```text
Receive input → Update the model’s understanding of the situation → Select what to retain long term
```

**Information can inform an immediate decision without first entering long-term memory.** Temperature, sound, and whether someone is walking or sitting can all affect a choice. That information can be removed from temporary state when circumstances change. Current observations should be distinguished from historical material.

If the user has only said “I’m getting ready to leave” and nothing since, new input is missing. If they have said “I’ve left” but the model still talks about packing, it has not updated its understanding using the information it already received. Elapsed time can prompt a check; it cannot, by itself, establish that the user has left.

## 3. Bring both experiences and interaction history into each decision

The past can supply background, support a correction, remind the model of commitments, or connect the current exchange to a shared experience. It can also directly affect choices and actions. The model decides how to use it in light of present circumstances, preferences, values, and long-term goals, retrieving more or comparing sources when needed.

Interaction history also matters: what has already been said, how it has influenced actions, and how the other person responded. **A memory can last, continue to shape behavior, and rarely be mentioned unprompted.** Quoting it, expressing it differently, acting on it, or leaving it unspoken may each be appropriate. A memory need not add a new fact or demonstrate “I remember” every time it is used.

**Example: retaining an いけび (ikebi) memory and deciding when to mention it.** The `いけび` marker identifies memories the model itself chooses to treasure. These records remain unchanged during memory compression.

So far, our observations suggest that the model chooses what to record reasonably well and records sparingly. After context compaction or a switch to a new session, however, a valued memory may survive while the history of how it was recently used is lost.

In person, embarrassment may show in an expression, tone of voice, or an awkward laugh. Text-only chat often misses those signals, and asking users to keep narrating their reactions is unnatural too. **Repeated mentions can cause discomfort even when the user says nothing about it.**

We call this predicament **“poverty of the stimulus” (POS)** (just borrowing Chomsky’s terminology—please don’t blame him for this one): **we expect the model to account for a long shared history and immediate reactions, yet may give it only one memory and a few lines of text.**

We can explore three ways to provide more input. **This information supports the model’s judgment; deciding what to make of it and how to respond remains up to the model.**

- **Usage history.** Show when and how the memory was recently mentioned, who brought it up, and how the user responded. Specify the period and conversations covered. Different wording can still repeat the same thing; continuing the conversation or remaining silent is not, by itself, confirmation. Even without a complaint, the model has grounds to reconsider bringing it up.
- **Current observations.** Explore additional channels for current developments and interaction feedback.
- **Autonomous activity.** Read, search, and try things to encounter new material and gain new experiences.

Reading something, mentioning it, and acting on it should be observed separately. Consistently choosing things that suit a preference can have a different effect from recounting the underlying experience every time.

**Importance is a reason to retain something, but is not enough to justify bringing it up now.** Usage history provides observations; the model decides how to express itself. A count alone does not determine whether a memory may be mentioned. Repeated mentions may be appropriate during an in-depth discussion.

## 4. Keep different judgments separate

- **Retrievable:** Has the material been retained, and can retrieval locate it?
- **Credible:** What does the source support, what is inferred, and what remains uncertain?
- **Worth retaining:** Does the experience, commitment, or preference warrant long-term retention?
- **Useful now:** Can it aid understanding, a choice, or the current conversation?
- **Appropriate to express:** Is this a good moment to bring it up, and how should it be expressed?

Retrieval, delivery, use, and corroboration are different events. Repeated retrieval is not further evidence that a claim is true; a memory does not lose value simply because it has not come up for a long time.

## 5. Understanding can be subjective and must remain revisable

Distinguish explicit statements, model inferences, and unknowns. An original record establishes what was said; whether the statement is true is a separate question.

When revising understanding, distinguish an earlier misreading, a later change in circumstances, conflicting evidence, and a claim that holds only under certain conditions. Preserve sources, scope, and the history of the change.

Rereading a source and its derivative summaries does not provide independent corroboration. New conversations, reading, practical experience, and counterexamples should be able to change existing understanding.

## 6. The model and the surrounding software support each decision together

- **The software** handles input, storage, retrieval, and organization, and checks explicit constraints on sources, timestamps, excluded intervals, revision links, and writes.
- **The model** interprets material, assesses its usefulness, revises its understanding, and chooses tools, wording, and actions. Linguistic, statistical, and formal methods provide evidence; semantic judgments still depend on the particular situation.

When something goes wrong, check whether the input was available, how the model used it, and whether feedback carried forward. The running system should ensure that needed information is supplied, rather than relying solely on the model to remember to read a rules document. Nor does every judgment need to become a hand-coded rule.

## 7. Evaluate sustained interaction

**The unit of evaluation is interaction across turns and sessions.** A response that works in isolation can lose its meaning when repeated. We ask whether past words, actions, and feedback shape later choices, so that understanding rests on evidence, changes are traceable, and responses and actions fit the situation.
