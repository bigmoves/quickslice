# Quickstart: Deploy on Railway

Get a quickslice instance running in a few minutes. By the end, you'll have:

- A live GraphQL API for your AT Protocol app
- OAuth authentication via AT Protocol identity
- Real-time data ingestion from the network

## Prerequisites

- **AT Protocol account** - A Bluesky account or other AT Protocol identity
- **Lexicon files** - JSON files defining your app's data schema

> **New to Lexicons?**
> Lexicons are JSON schemas that define record types in AT Protocol (similar to
> database tables). Each lexicon has an NSID like `com.example.post`. See the
> [AT Protocol Lexicon docs](https://atproto.com/specs/lexicon) to create yours.

## 1. Deploy to Railway

Click the button below to create a new quickslice instance:

[![Deploy on Railway](https://railway.com/button.svg)](https://railway.com/deploy/quickslice?referralCode=Ofii6e&utm_medium=integration&utm_source=template&utm_campaign=generic)

Railway will prompt you to configure environment variables. Leave the form open. You'll need to generate a signing key next.

## 2. Generate OAuth Signing Key

Quickslice needs a private key to sign OAuth tokens. You'll generate this using `goat`, a CLI for AT Protocol.

**Install goat:**

```bash
brew install goat
```

> **What is goat?**
> [Goat](https://github.com/bluesky-social/indigo/tree/main/cmd/goat) is a
> general-purpose CLI for AT Protocol operations: key generation, identity
> resolution, record inspection, and more.

**Generate the key:**

```bash
goat key generate -t p256
```

This outputs a P-256 private key. Copy the entire output.

> **Why P-256?**
> AT Protocol's OAuth implementation uses DPoP (Demonstrating Proof of Possession)
> to bind tokens to a specific key. The P-256 elliptic curve is required for
> signing these proofs.

**Paste into Railway:**

1. Paste the key into the `OAUTH_SIGNING_KEY` field
2. Click **Save Config**

## 3. Configure Your Domain

After saving the config, Railway deploys your instance. Once deployment completes:

1. Click on your quickslice service in the deployment canvas
2. Go to **Settings**
3. Click **Generate Domain** under the Networking section

Railway creates a public URL (e.g., `quickslice-production-xxxx.up.railway.app`).

**Redeploy to apply the domain:**

1. Go to the **Deployments** tab
2. Click the three-dot menu on the latest deployment
3. Select **Redeploy**

This ensures quickslice knows its public URL for OAuth redirects.

## 4. Create Admin Account

Visit your new domain. You'll see a welcome screen prompting you to create an admin account.

1. Enter your AT Protocol handle (e.g., `yourname.bsky.social`)
2. Click **Authenticate**
3. You'll be redirected to your PDS to authorize quickslice
4. After authorization, you're returned to the quickslice homepage as admin

> **What just happened?**
> Quickslice authenticated you via AT Protocol OAuth. Your PDS verified your
> identity, and quickslice now recognizes your DID as the instance admin.

## 5. Configure Your Instance

From the quickslice homepage, go to **Settings**.

**Set your Domain Authority:**

Enter your domain authority in reverse-domain format (e.g., `social.grain`).

> **What is Domain Authority?**
> Domain authority is the namespace prefix for your lexicons. If your lexicons
> are named `social.grain.photo` and `social.grain.like`, your domain authority
> is `social.grain`. This must match the prefix in your lexicon files.

Click **Save**.

**Upload your Lexicons:**

Prepare a `.zip` file containing your lexicon JSON files:

```
lexicons.zip
├── social.grain.photo.json
├── social.grain.like.json
└── social.grain.follow.json
```

Each file should be a valid [Lexicon schema](https://atproto.com/specs/lexicon).

1. Click **Upload Lexicons**
2. Select your `.zip` file
3. Quickslice validates and registers each lexicon

> **Validation Errors?**
> Lexicon schemas are validated on import and will fail if incorrectly defined.
> If you encounter validation errors, try [glot](https://tangled.org/bnewbold.net/cobalt/tree/main/cmd/glot)
> to check your schemas locally before uploading.

## 6. Backfill Network Data

Return to the quickslice homepage and click **Trigger Backfill**.

This scans the AT Protocol network for existing records matching your lexicons and imports them into your instance.

> **Monitoring Progress**
> Backfill duration depends on how many matching records exist across the network.
> You can monitor progress in the Railway logs (Deployments → View Logs). The page
> will refresh when backfill completes.

## 7. Start Querying

Once backfill finishes, click **Open GraphiQL** to access the interactive GraphQL explorer.

Try a sample query:

```graphql
query {
  socialGrainPhoto(first: 10) {
    edges {
      node {
        photo {
          url
        }
        createdAt
      }
    }
  }
}
```

Your quickslice instance is now live.
