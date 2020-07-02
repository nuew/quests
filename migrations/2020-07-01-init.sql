CREATE TYPE chat_role AS ENUM ('Owner', 'Moderator', 'Voiced');
CREATE TYPE electionsys AS ENUM ('Approval', 'Borda', 'FPTP', 'Schulze', 'STV');
CREATE TYPE passagetype AS ENUM ('Chat', 'Dice', 'Poll', 'Textual');
CREATE TYPE quest_role AS ENUM ('Author', 'Participant', 'Guest');
CREATE TYPE shelf_role AS ENUM ('Editor', 'Visitor');
CREATE TYPE visibility AS ENUM ('Public', 'Unlisted', 'Private');

-- Users
CREATE TABLE users (
    id              SERIAL      PRIMARY KEY,
    name            TEXT        NOT NULL,
    email           TEXT        NOT NULL,
    avatar          TEXT        NULL,
    created_at      TIMESTAMPTZ NOT NULL DEFAULT now(),

    password_hash   BYTEA       NOT NULL,

    biography       TEXT        NOT NULL DEFAULT '',
    location        TEXT        NOT NULL DEFAULT '',
    pronouns        TEXT        NOT NULL DEFAULT '',
    website         TEXT        NOT NULL DEFAULT ''
);

CREATE TABLE user_slugs (
    slug            TEXT        NOT NULL PRIMARY KEY,
    user_id         INTEGER     NOT NULL REFERENCES users
                                    ON DELETE CASCADE,
    created_at      TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE TABLE sessions (
    id              SERIAL      PRIMARY KEY,
    user_id         INTEGER     NOT NULL REFERENCES users
                                    ON DELETE CASCADE,
    ip_addr         INET        NOT NULL,
    user_agent      TEXT        NOT NULL,
    created_at      TIMESTAMPTZ NOT NULL DEFAULT now(),
    last_active     TIMESTAMPTZ NOT NULL DEFAULT now(),
    expires_at      TIMESTAMPTZ NULL
);

-- Chats
CREATE TABLE chats (
    id              SERIAL      PRIMARY KEY,
    topic           TEXT        NOT NULL,
    open            BOOLEAN     NOT NULL DEFAULT True,
    voiced_only     BOOLEAN     NOT NULL DEFAULT False
);

CREATE TABLE chat_roles (
    chat_id         INT         NOT NULL REFERENCES chats
                                    ON DELETE CASCADE,
    user_id         INT         NOT NULL REFERENCES users
                                    ON DELETE CASCADE,
    role            CHAT_ROLE   NOT NULL,

    PRIMARY KEY (chat_id, user_id)
);

CREATE TABLE messages (
    id              SERIAL      PRIMARY KEY,
    chat_id         INT         NOT NULL REFERENCES chats
                                    ON DELETE CASCADE,
    user_id         INT         NOT NULL REFERENCES users,
    posted_at       TIMESTAMPTZ NOT NULL DEFAULT now(),
    last_updated    TIMESTAMPTZ NOT NULL DEFAULT now(),
    message         TEXT        NOT NULL,
    attachment      TEXT        NULL
);

-- Polls
CREATE TABLE polls (
    id              SERIAL      PRIMARY KEY,
    system          ELECTIONSYS NOT NULL,
    prompt          TEXT        NOT NULL,
    open            BOOLEAN     NOT NULL DEFAULT True,
    winners         INT         NOT NULL DEFAULT 1 CHECK (winners > 0),
    submissions     BOOLEAN     NOT NULL DEFAULT True
);

CREATE TABLE poll_choices (
    id              SERIAL      PRIMARY KEY,
    poll_id         INT         NOT NULL REFERENCES polls
                                    ON DELETE CASCADE,
    user_id         INT         NOT NULL REFERENCES users,
    choice          TEXT        NOT NULL,
    cancel_reason   TEXT        NULL, -- NULL if not cancelled
    created_at      TIMESTAMPTZ NOT NULL DEFAULT now(),
    last_updated    TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE TABLE poll_ballot_choice (
    choice_id       INT         NOT NULL REFERENCES poll_choices
                                    ON DELETE CASCADE,
    user_id         INT         NOT NULL REFERENCES users
                                    ON DELETE CASCADE,
    ranking         SMALLINT    NULL, -- NULL if the election method in use doesn't use rankings.

    PRIMARY KEY (choice_id, user_id)
);

-- Quests
CREATE TABLE quests (
    id              SERIAL      PRIMARY KEY,
    name            TEXT        NOT NULL,
    teaser          TEXT        NOT NULL,
    banner          TEXT        NULL,
    visibility      VISIBILITY  NOT NULL,
    only_approved   BOOLEAN     NOT NULL,
    created_at      TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE TABLE quest_roles (
    quest_id        INT         NOT NULL REFERENCES quests
                                    ON DELETE CASCADE,
    user_id         INT         NOT NULL REFERENCES users
                                    ON DELETE CASCADE,
    role            QUEST_ROLE  NOT NULL,

    PRIMARY KEY (quest_id, user_id)
);

CREATE TABLE chapters (
    id              SERIAL      PRIMARY KEY,
    quest_id        INT         NOT NULL REFERENCES quests
                                    ON DELETE CASCADE,
    name            TEXT        NOT NULL,
    position        SMALLINT    NOT NULL,
    is_appendix     BOOLEAN     NOT NULL,

    UNIQUE (quest_id, position, is_appendix)
);

CREATE TABLE passages (
    id              SERIAL      PRIMARY KEY,
    passagetype     PASSAGETYPE NOT NULL,
    chapter_id      INT         NOT NULL REFERENCES chapters
                                    ON DELETE CASCADE,
    position        SMALLINT    NOT NULL,
    created_at      TIMESTAMPTZ NOT NULL DEFAULT now(),
    last_updated    TIMESTAMPTZ NOT NULL DEFAULT now(),

    UNIQUE (id, passagetype),
    UNIQUE (chapter_id, position)
);

CREATE TABLE chat_passages (
    id              INT         PRIMARY KEY,
    passagetype     PASSAGETYPE NOT NULL DEFAULT 'Chat'
                                    CHECK (passagetype = 'Chat'),
    chat_id         INT         NOT NULL REFERENCES chats,

    FOREIGN KEY (id, passagetype) REFERENCES passages (id, passagetype)
);

CREATE TABLE dice_passages (
    id              INT         PRIMARY KEY,
    passagetype     PASSAGETYPE NOT NULL DEFAULT 'Dice'
                                    CHECK (passagetype = 'Dice'),
    best_of         SMALLINT    NOT NULL CHECK (best_of > 0),
    quantity        SMALLINT    NOT NULL CHECK (quantity > 0),
    sides           SMALLINT    NOT NULL CHECK (sides > 0),
    threshold       SMALLINT    NULL CHECK (threshold > 0),

    FOREIGN KEY (id, passagetype) REFERENCES passages (id, passagetype)
);

CREATE TABLE dice_rolls (
    id              SERIAL      PRIMARY KEY,
    passage_id      INT         NOT NULL REFERENCES dice_passages
                                    ON DELETE CASCADE,
    user_id         INT         NOT NULL REFERENCES users,
    values          SMALLINT[]  NOT NULL, -- Denormalized because normalizing this would be stupid.
    position        SMALLINT    NOT NULL
);

CREATE TABLE poll_passages (
    id              INT         PRIMARY KEY,
    passagetype     PASSAGETYPE NOT NULL DEFAULT 'Poll'
                                    CHECK (passagetype = 'Poll'),
    poll_id         INT         NOT NULL REFERENCES polls,

    FOREIGN KEY (id, passagetype) REFERENCES passages (id, passagetype)
);

CREATE TABLE textual_passages (
    id              INT         PRIMARY KEY,
    passagetype     PASSAGETYPE NOT NULL DEFAULT 'Textual'
                                    CHECK (passagetype = 'Textual'),
    contents        TEXT        NOT NULL,

    FOREIGN KEY (id, passagetype) REFERENCES passages (id, passagetype)
);

CREATE VIEW absorbed_passages AS
    SELECT p.*, c.chat_id, d.best_of, d.quantity, d.sides, d.threshold,
        v.poll_id, t.contents
    FROM passages p
    LEFT JOIN chat_passages c ON p.id = c.id
    LEFT JOIN dice_passages d ON p.id = d.id
    LEFT JOIN poll_passages v ON p.id = v.id
    LEFT JOIN textual_passages t ON p.id = t.id
    WHERE
        c.id IS NOT NULL OR
        d.id IS NOT NULL OR
        v.id IS NOT NULL OR
        t.id IS NOT NULL;

-- Tags
CREATE TABLE tags (
    id              SERIAL      PRIMARY KEY,
    name            TEXT        NOT NULL UNIQUE
);

CREATE TABLE quest_tags (
    tag_id          INT         NOT NULL REFERENCES tags
                                    ON DELETE CASCADE,
    quest_id        INT         NOT NULL REFERENCES quests
                                    ON DELETE CASCADE,

    PRIMARY KEY (tag_id, quest_id)
);

-- Bookshelves
CREATE TABLE bookshelves (
    id              SERIAL      PRIMARY KEY,
    owner           INT         NOT NULL REFERENCES users
                                    ON DELETE CASCADE,
    name            TEXT        NOT NULL,
    description     TEXT        NOT NULL,
    icon            CHAR        NOT NULL,
    email_updates   BOOLEAN     NOT NULL,
    visibility      VISIBILITY  NOT NULL,
    created_at      TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE TABLE bookshelf_roles (
    bookshelf_id    INT         NOT NULL REFERENCES bookshelves
                                    ON DELETE CASCADE,
    user_id         INT         NOT NULL REFERENCES users
                                    ON DELETE CASCADE,
    role            SHELF_ROLE  NOT NULL,

    PRIMARY KEY (bookshelf_id, user_id)
);

CREATE TABLE bookshelf_items (
    bookshelf_id    INT         NOT NULL REFERENCES bookshelves
                                    ON DELETE CASCADE,
    quest_id        INT         NOT NULL REFERENCES quests
                                    ON DELETE CASCADE,
    added_at        TIMESTAMPTZ NOT NULL DEFAULT now(),

    PRIMARY KEY (bookshelf_id, quest_id)
);

-- Bans
CREATE TABLE bans (
    id              SERIAL      PRIMARY KEY,
    user_id         INT         NOT NULL REFERENCES users
                                    ON DELETE CASCADE,
    created_at      TIMESTAMPTZ NOT NULL DEFAULT now(),
    created_by      INT         NOT NULL REFERENCES users,
    expires_at      TIMESTAMPTZ NULL, -- NULL for never
    reason          TEXT        NOT NULL,
    global          BOOLEAN     NOT NULL DEFAULT False
);

CREATE TABLE quest_bans (
    ban_id          INT         NOT NULL REFERENCES bans
                                    ON DELETE CASCADE,
    quest_id        INT         NOT NULL REFERENCES quests
                                    ON DELETE CASCADE,

    PRIMARY KEY (ban_id, quest_id)
);

CREATE TABLE chat_bans (
    ban_id          INT         NOT NULL REFERENCES bans
                                    ON DELETE CASCADE,
    chat_id         INT         NOT NULL REFERENCES chats
                                    ON DELETE CASCADE,

    PRIMARY KEY (ban_id, chat_id)
);

CREATE VIEW active_quest_bans AS
    SELECT b.*, q.quest_id
    FROM bans b, quest_bans q
    WHERE now() < b.expires_at AND (b.global = True OR b.id = q.ban_id);

CREATE VIEW active_chat_bans AS
    SELECT b.*, c.chat_id
    FROM bans b, chat_bans c
    WHERE now() < b.expires_at AND (b.global = True OR b.id = c.ban_id);
