# Expert Discussion: Enhanced InForm — Usefulness, Direction, and Context

**Purpose:** A structured, multi-perspective discussion on the value of an enhanced InForm, what it should prioritize, how it relates to classic vs. modern visual development, AI-assisted development, and related concerns.  
**Format:** Fictitious roundtable; viewpoints are representative of common positions in industry and education, not endorsements.  
**Context:** InForm is a VB-style WYSIWYG UI designer and runtime for QB64; see [INFORM_FUNCTIONALITY.md](INFORM_FUNCTIONALITY.md).

---

## Participants (Personas)

| Name  | Focus |
|-------|--------|
| **Maya**  | Modern frontend, UX, and design systems; React/declarative tooling. |
| **David** | Legacy systems, maintainability, and “good enough” tooling for internal/edu apps. |
| **Jin**   | AI-assisted development, code generation, and low-code. |
| **Sam**   | QB64/BASIC community: hobbyists, educators, retro, gamedev. |
| **Riley** | Software architecture, build vs. buy, and project sustainability. |

---

## 1. How useful would an enhanced InForm be?

**Maya:** I’m skeptical. The industry moved on from form designers for good reasons. Declarative UI (React, SwiftUI, Flutter, Compose) gives you layout as code, components, design systems, and real tooling. Recreating VB6’s form designer in 2025 feels like rebuilding a fax machine. Who is the user? If they want a quick dialog, they can use a no-code SaaS or a small web form. If they want to learn “real” UI, they should learn components and state.

**David:** The user is someone who doesn’t want to learn React or Flutter. There’s a huge population: K–12 and intro-CS teachers, hobbyists, people maintaining small business tools, retrocomputing enthusiasts. For “put a button and a text box on a form and run it,” a VB-style designer is still one of the lowest-friction paths. It’s useful because the *cognitive load* is low: you see the form, you click, you get an event. No JS build, no npm, no components. The question isn’t “is it the best?” It’s “does it serve a real cohort?” I think yes.

**Jin:** Usefulness shifts when you add AI. Form designers produce *structured* output: a .frm with controls and properties, and stubs. That’s very easy for an LLM to read, modify, and extend. A prompt like “add a listbox that shows the selected item in the label” maps cleanly onto InForm’s control set and events. The usefulness of an enhanced InForm goes up if we treat it as a *target* for AI-generated UI: the form is the schema, the stubs are the insertion points. So usefulness isn’t only “human drag-drop” — it’s “human + AI co-design.”

**Sam:** For the QB64 community, it’s already useful. People use InForm for config dialogs, launchers, small utilities. The pain points are: no multi-form project, no Timer in the box, no Drive/Dir/File controls, preview is separate. An *enhanced* InForm that fixes those and stays within the BASIC ecosystem would get adoption. Usefulness = “can I ship a small GUI without leaving QB64.” That’s the bar.

**Riley:** Usefulness also depends on *who builds it* and *who maintains it*. If “enhanced InForm” means a sidebar inside a compiler project whose main job is “compile BASIC to C,” we have to ask: is that scope creep? The compiler doesn’t need a form designer to succeed. So: useful *to the QB64 ecosystem* — likely yes. Useful *as a first-class deliverable of a compiler project* — only if it’s clearly scoped, and preferably if it stays a separate, composable layer (e.g. InForm-PE as a dependency, not forked into the compiler repo).

---

## 2. What should an enhanced InForm have?

**David:** I’d prioritize in this order: (1) **Multi-form project** — .vbp-like or a simple manifest so you can have a main form and dialogs. (2) **Timer and Drive/Dir/File controls** — they’re in every “my first VB” tutorial; their absence is felt. (3) **Live design surface** — the form *is* the designer, not a TCP preview. (4) **Integrated editor** — designer + code in one process, even if it’s “click form tab / click code tab.” After that: Shape/Line, then data binding.

**Maya:** If we’re doing it, don’t replicate VB’s mistakes. Add **layout constraints** — “this group stays at the bottom,” “these buttons are in a row that grows.” Visual form designers of the ’90s produced rigid pixel layouts; when you resize, everything breaks. Even a simple anchor or “stick to right/bottom” would help. And **accessibility**: roles, names, keyboard nav. Classic VB didn’t think about that; we should.

**Jin:** For AI interop: **machine-readable .frm** — already mostly there, but a clear, versioned schema helps. **Stub conventions** — `ControlName_EventName` is good; consistent patterns for “where do I put validation” or “where do I load data” would help code-gen. **Property hints** — which properties are “design-only” vs. “runtime too,” so the model doesn’t emit nonsense. I’d also add a **CLI or headless mode** for the designer: “generate a form from this spec” so an AI or script can produce .frm without a GUI.

**Sam:** From the community side: **docs and examples**. InForm is powerful but under-documented. A “InForm in 5 minutes” and a small set of reference forms (login, settings, file picker) would multiply usefulness. **One-click run** — from the designer, “run in QB64” without manually opening the IDE. And **backwards compatibility** — enhanced InForm should still load existing .frm; otherwise we fracture the user base.

**Riley:** I’d add **explicit boundaries**. A “Phase 1 enhanced InForm” could be: multi-form project, Timer, Drive/Dir/File, live design surface, and integrated designer+editor in one exe. No data binding, no MDI, no Shape/Line in v1. That’s a bounded scope. Phase 2: data binding, more controls, accessibility. The worst outcome is “we want to match VB6” — that’s unbounded.

---

## 3. Classic visual dev vs. modern tooling

**Maya:** Modern tooling won. The reason is **iteration and reuse**. In React you have components, hooks, and a clear data flow. Form designers give you one-off forms and magic strings. I don’t think “echoing” VB is a good *goal*. The goal should be “lower the barrier for our users.” If that’s best done with a form designer, fine — but the *design* of that designer can borrow from modern ideas: constraints, tokens, a design system. Don’t fossilize the UX of 1995.

**David:** “Echoing” has a different value: **conceptual continuity**. People who used VB 20 years ago can sit down and understand InForm in minutes. The same is true for teachers who learned on VB and now use QB64. There’s a pedagogic and cognitive benefit to “it works like the thing you already know.” That doesn’t mean we can’t add constraints or better keyboard nav — but the *metaphor* (form, controls, events, property grid) is worth preserving. Modern tooling is optimized for teams and scale; our users often aren’t.

**Jin:** The split isn’t classic vs. modern — it’s **imperative form-centric** vs. **declarative component-centric**. Form designers are imperative in spirit: “I placed this here, then I handle this event.” AI and modern tooling are shifting to declarative: “here’s the structure and state; the framework renders it.” An enhanced InForm could sit in the middle: the *designer* is classic (drag-drop, properties), but the *serialization* could be more declarative (e.g. a .frm that’s JSON or a well-defined DSL). That would make it easier for AI and for future tooling to parse and generate. So: keep the classic *interaction* for humans, modernize the *representation*.

**Sam:** For our community, classic is an advantage. QB64 is already “BASIC with modern runtimes” — it’s *supposed* to feel familiar. If we made InForm look and act like Figma or a React designer, we’d lose the “I already get this” factor. I’m fine with a better property grid, snap-to-guides, and constraints — but the mental model should stay: form, toolbox, properties, events.

**Riley:** The framing “classic vs. modern” is a false choice. We’re really choosing **audience and constraint*. If the audience is “people who want something like VB” and the constraint is “runs on QB64 and produces BASIC,” then a classic-style form designer is fit-for-purpose. We’re not competing with React or Flutter; we’re serving a different segment. The risk is *pretending* we’re modern by bolting on buzzwords (e.g. “reactive binding”) without the rest of the stack. Better: do the classic model well, and adopt modern *practices* (accessibility, machine-readable formats, tests) under the hood.

---

## 4. How does this relate to AI development?

**Jin:** Strongly. Form designers are **structure-rich**. An LLM can: (1) *Generate* a .frm from a natural-language spec. (2) *Edit* an existing .frm (“add a dropdown for country selection”). (3) *Fill* event stubs from descriptions (“when the user clicks Submit, validate the email and then send”). The .frm + stubs are a good **contract**: the model doesn’t have to invent layout or control names; it follows the schema. An enhanced InForm that exposes a clean schema, maybe an optional JSON/DSL export, and consistent naming would be very AI-friendly. The designer could even have an “AI assist” pane: “Describe the change” → diff against .frm and .bas.

**Maya:** AI can *also* generate React or Flutter. The question is whether we’re optimizing for “AI generates InForm” or “AI generates whatever.” If the goal is to use AI well, maybe we should spend effort on “AI generates clean BASIC” in general — good LSP, symbol info, quick-fixes — and let the form designer be one of many targets. I’d be wary of over-investing in “AI-specific” InForm features until we see real usage.

**David:** I like the idea of InForm as a **target** for AI. The reason: **scope**. Generating a full React app is open-ended. Generating “a form with these controls and these stubs” is closed. The model has a clear stopping point and a verifiable output. For education, that’s useful: “AI, make me a quiz form” → student fills in the logic. The form is the scaffold; AI does the boilerplate. So the relation is: enhanced InForm should be **easy to generate and to modify by machine**, without requiring the human to understand the innards.

**Sam:** The QB64 ecosystem doesn’t have a lot of AI tooling yet. An InForm that plays well with “describe a form, get .frm + stubs” could be a differentiator. It’s also a good entry point for people who are scared of code: they describe, they get a form, they tweak in the designer, they fill one or two events. AI lowers the barrier; the form designer makes the result editable and runnable.

**Riley:** The relationship to AI is **opportunistic**. We shouldn’t design InForm *for* AI first — we should design it for humans. If we do that well (clear schema, consistent stubs, maybe a headless/CLI mode), AI compatibility is a by-product. Don’t let “AI-friendly” drive the core design; let it influence formats and conventions.

---

## 5. Other topics

### 5.1 Education

**David:** Form-based UIs are still how many intro courses teach “events” and “state” in a contained way. One form, a few controls, a few handlers — that’s a full loop. InForm fits that. An enhanced InForm with multi-form and Timer could support “build a small game menu” or “build a settings dialog” as assignments. The key is **predictable behavior** and **good error messages** so students aren’t blocked by tooling.

**Sam:** Schools that have old VB licenses or that moved to QB64 for cost reasons would use an enhanced InForm. The fact that it’s free and cross-platform matters. We should have a “teaching” section in the docs: common assignments, pitfalls, and “from zero to a running form” in one class period.

### 5.2 Accessibility

**Maya:** If we’re building a GUI toolkit in 2025, we have to think about screen readers, keyboard-only use, and focus. InForm’s custom-drawn controls don’t use native a11y by default. An enhanced InForm should at least: (1) **Expose roles and names** (e.g. for future SDL or platform a11y hooks). (2) **Sensible tab order** and a way to set it in the designer. (3) **Document** what’s supported so users know the limits. We might not get full a11y in v1, but we shouldn’t ignore it.

**Riley:** Agreed. Treat it as a requirement for “Phase 2” or as a non-negotiable for any new control we add. Don’t block Phase 1 on full a11y, but don’t design in a way that makes it impossible later.

### 5.3 Scope and sustainability

**Riley:** The main risk is scope creep. An “enhanced InForm” could mean: (a) a set of patches and a spec that the InForm-PE maintainers adopt, (b) a fork maintained by the QB64Fresh (or QB64pe) project, or (c) a from-scratch reimplementation. (a) is most sustainable if the upstream is willing. (c) is a multi-year project. The compiler project should not *own* the designer long-term; it should *integrate* with it (e.g. “open in InForm” from the IDE, or ship InForm as a recommended add-on). Ownership and maintenance need to be explicit.

**Sam:** The InForm-PE community is small. If we want “enhanced,” we might need to contribute there or host a well-defined fork with a clear “we maintain this” statement. Otherwise it becomes abandonware.

### 5.4 Native vs. custom-drawn

**Maya:** InForm’s custom-drawn, themed controls are both a strength (consistent look, theming) and a weakness (no native a11y, no native IME, no “it just works like the OS”). An enhanced InForm could explore **hybrid**: use native controls where the platform provides them (e.g. edit boxes, lists) and custom-draw only where we need consistency (buttons, frames). That’s a big refactor, so it’s a “maybe later” — but it’s worth not baking in “everything is custom-drawn forever.”

**David:** Custom-drawn is part of InForm’s cross-platform story: same look on Windows, Linux, Mac. If we go hybrid, we have to decide: do we want native (and accept look-and-feel differences) or consistent (and accept a11y/IME as a separate problem)? For an enhanced v1, I’d keep custom-drawn and invest in a11y and keyboard nav within that model. Revisit hybrid if we hit hard limits.

---

## 6. Consensus

After discussion, the group agrees on the following.

### 6.1 Usefulness

- An **enhanced InForm is useful** for: QB64 users who want low-friction GUIs, educators teaching event-driven UIs, hobbyists and small tools, and as a **machine-friendly target** for AI-generated or script-generated forms. It is **not** a replacement for modern frameworks; it serves a different segment.
- Usefulness is **multiplied** by: multi-form projects, a few high-value controls (Timer, Drive/Dir/File), a live design surface, and better docs/examples. Usefulness is **reduced** if the enhanced version is unstable, incompatible with existing .frm, or unmaintained.

### 6.2 What it should have (priority)

1. **Multi-form project** (manifest or project file).
2. **Timer, DriveListBox, DirListBox, FileListBox** in the standard set.
3. **Live design surface** — form as designer, not only TCP preview.
4. **Designer + code in one process** (tabs or panes), and **one-click run**.
5. **Machine-readable, versioned .frm** (and optionally a JSON/DSL export) for AI and tooling.
6. **Accessibility baseline**: roles/names where feasible, configurable tab order, and documentation of limits. Full a11y as Phase 2.
7. **Layout constraints or anchors** (e.g. “stick to right/bottom”) to improve resize behavior; need not be as rich as modern constraint systems.
8. **Backwards compatibility** with existing .frm.
9. **Clear boundaries**: Phase 1 vs. Phase 2; no unbounded “match VB6” scope.

### 6.3 Classic vs. modern

- **Keep the classic metaphor** (form, toolbox, property grid, events) for learnability and continuity with VB and with the current InForm user base.
- **Adopt modern practices** where they don’t conflict: accessibility, machine-readable formats, clearer layout semantics (constraints/anchors), and under-the-hood quality (tests, docs). The *interaction* can stay classic; the *representation* and *practices* can modernize.

### 6.4 AI development

- **Treat InForm as an AI-friendly target**: clear schema, consistent stubs, and (optionally) headless/CLI generation. Don’t design *for* AI first; design for humans, and make the output easy for machines to parse and modify. “AI assist” in the designer is a reasonable later feature, not a v1 requirement.

### 6.5 Scope and ownership

- **The compiler project should not own the form designer long-term.** Prefer: contribute to InForm-PE, or maintain a well-scoped fork with a clear maintainer. The compiler/IDE can *integrate* (e.g. “Open in InForm,” or ship InForm as a recommended add-on). Ownership and maintenance must be explicit to avoid abandonware.

### 6.6 Summary sentence

> An enhanced InForm is **worth pursuing** for the QB64 ecosystem, with a **bounded Phase 1** (multi-form, Timer, file/dir controls, live designer, integrated editor, machine-readable formats, and an accessibility baseline). It should **preserve the classic form-based metaphor** while adopting **modern practices** (a11y, schemas, constraints) and remaining **AI-friendly by design**. **Ownership and scope** should be clearly defined, with the form designer as an **integrated partner** to the compiler, not an unbounded sub-project.

---

## 7. References

- [INFORM_FUNCTIONALITY.md](INFORM_FUNCTIONALITY.md) — InForm feature and runtime documentation.
- [INFORM_FUNCTIONALITY.md §7](INFORM_FUNCTIONALITY.md#7-comparison-with-visual-basic-and-similar-classic-tools) — Comparison with Visual Basic and similar tools.
