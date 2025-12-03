# OAuth Client Type Dropdown Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add a Public/Confidential dropdown to the OAuth client registration form so users can choose the client type.

**Architecture:** The message handler and state already exist - just need to add the UI dropdown element to the form and display the client type in the client card view.

**Tech Stack:** Gleam, Lustre (frontend framework), existing UI patterns.

---

## Task 1: Add Client Type Dropdown to New Client Form

**Files:**
- Modify: `client/src/pages/settings.gleam:466-516`

**Step 1: Add the dropdown after Client Name input**

Find this section (around line 480-481):

```gleam
          event.on_input(UpdateNewClientName),
        ]),
        // Redirect URIs
```

Add the Client Type dropdown between Client Name and Redirect URIs:

```gleam
          event.on_input(UpdateNewClientName),
        ]),
        // Client Type dropdown
        html.div([], [
          html.label([attribute.class("block text-sm text-zinc-400 mb-1")], [
            element.text("Client Type"),
          ]),
          html.select(
            [
              attribute.class(
                "font-mono px-3 py-2 text-sm text-zinc-300 bg-zinc-800 border border-zinc-700 rounded w-full",
              ),
              event.on_input(UpdateNewClientType),
            ],
            [
              html.option(
                [
                  attribute.value("PUBLIC"),
                  attribute.selected(model.new_client_type == "PUBLIC"),
                ],
                "Public",
              ),
              html.option(
                [
                  attribute.value("CONFIDENTIAL"),
                  attribute.selected(model.new_client_type == "CONFIDENTIAL"),
                ],
                "Confidential",
              ),
            ],
          ),
          html.p([attribute.class("text-xs text-zinc-500 mt-1")], [
            element.text(case model.new_client_type {
              "CONFIDENTIAL" ->
                "For server-side apps that can securely store a client secret"
              _ ->
                "For browser apps (SPAs) and mobile apps that cannot securely store a secret"
            }),
          ]),
        ]),
        // Redirect URIs
```

**Step 2: Build to verify syntax**

Run: `cd client && gleam build`
Expected: Compiles successfully

**Step 3: Commit**

```bash
git add client/src/pages/settings.gleam
git commit -m "feat(oauth): add client type dropdown to registration form"
```

---

## Task 2: Add Client Type Display to Client Card

**Files:**
- Modify: `client/src/pages/settings.gleam:579-587`

**Step 1: Add client type display after Client ID**

Find this section (around line 586-588):

```gleam
            element.text(client.client_id),
          ]),
          // Client Secret (if confidential)
```

Add client type display between Client ID and Client Secret:

```gleam
            element.text(client.client_id),
          ]),
          // Client Type
          html.div([attribute.class("mb-2")], [
            html.span([attribute.class("text-xs text-zinc-500")], [
              element.text("Type: "),
            ]),
            html.span([attribute.class("text-xs text-zinc-400")], [
              element.text(case client.client_type {
                "PUBLIC" -> "Public"
                "CONFIDENTIAL" -> "Confidential"
                _ -> client.client_type
              }),
            ]),
          ]),
          // Client Secret (if confidential)
```

**Step 2: Build to verify syntax**

Run: `cd client && gleam build`
Expected: Compiles successfully

**Step 3: Manual verification**

Run: `cd client && gleam run`
Expected:
- New client form shows Client Type dropdown with helper text
- Existing clients show their type in the card view

**Step 4: Commit**

```bash
git add client/src/pages/settings.gleam
git commit -m "feat(oauth): display client type in client card"
```

---

## Summary

| Task | Description | Files |
|------|-------------|-------|
| 1 | Add client type dropdown to new client form | `client/src/pages/settings.gleam` |
| 2 | Display client type in client card view | `client/src/pages/settings.gleam` |

**Total commits:** 2 focused commits following conventional commit format.
