CREATE TYPE chat_role AS ENUM ('Moderator', 'Voiced');
CREATE TYPE quest_role AS ENUM ('Author', 'Participant', 'Guest');
CREATE TYPE visibility AS ENUM ('Public', 'Unlisted', 'Private');
CREATE TYPE passagetype AS ENUM ('Chat', 'Dice', 'Poll', 'Textual');

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
    allow_anon      BOOLEAN     NOT NULL,
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
    user_id         INT         NULL REFERENCES users
                                    ON DELETE SET NULL,
    session_id      INT         NOT NULL REFERENCES sessions
                                    ON DELETE RESTRICT,
    posted_at       TIMESTAMPTZ NOT NULL DEFAULT now(),
    last_updated    TIMESTAMPTZ NULL,
    message         TEXT        NOT NULL,
    attachment      TEXT        NULL
);

-- Quests
CREATE TABLE quests (
    id              SERIAL      PRIMARY KEY,
    name            TEXT        NOT NULL,
    slug            TEXT        NOT NULL,
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
    position        INT         NOT NULL,
    is_appendix     BOOLEAN     NOT NULL,

    UNIQUE (quest_id, position, is_appendix)
);

CREATE TABLE passages (
    id              SERIAL      PRIMARY KEY,
    passagetype     PASSAGETYPE NOT NULL,
    chapter_id      INT         NOT NULL REFERENCES chapters
                                    ON DELETE CASCADE,
    position        INT         NOT NULL,
    created_at      TIMESTAMPTZ NOT NULL DEFAULT now(),
    last_updated    TIMESTAMPTZ NOT NULL,

    UNIQUE (id, passagetype),
    UNIQUE (chapter_id, position)
);

CREATE TABLE chat_passages (
    id              INT         PRIMARY KEY,
    passagetype     PASSAGETYPE NOT NULL DEFAULT 'Chat'
                                    CHECK (passagetype = 'Chat'),
    chat_id         INT         NOT NULL REFERENCES chats
                                    ON DELETE RESTRICT,

    FOREIGN KEY (id, passagetype) REFERENCES passages (id, passagetype)
);

CREATE TABLE dice_passages (
    id              INT         PRIMARY KEY,
    passagetype     PASSAGETYPE NOT NULL DEFAULT 'Dice'
                                    CHECK (passagetype = 'Dice'),
    best_of         SMALLINT    NOT NULL,
    quantity        SMALLINT    NOT NULL,
    sides           SMALLINT    NOT NULL,
    threshold       SMALLINT    NOT NULL,

    FOREIGN KEY (id, passagetype) REFERENCES passages (id, passagetype)
);

CREATE TABLE dice_rolls (
    id              SERIAL      PRIMARY KEY,
    passage_id      INT         NOT NULL REFERENCES dice_passages
                                    ON DELETE CASCADE,
    user_id         INT         NULL REFERENCES users
                                    ON DELETE SET NULL,
    values          SMALLINT[]  NOT NULL, -- Denormalized because normalizing this would be stupid.
    position        INT         NOT NULL
);

CREATE TABLE poll_passages (
    id              INT         PRIMARY KEY,
    passagetype     PASSAGETYPE NOT NULL DEFAULT 'Dice'
                                    CHECK (passagetype = 'Dice'),

    FOREIGN KEY (id, passagetype) REFERENCES passages (id, passagetype)
);

CREATE TABLE textual_passages (
    id              INT         PRIMARY KEY,
    passagetype     PASSAGETYPE NOT NULL DEFAULT 'Textual'
                                    CHECK (passagetype = 'Textual'),
    contents        TEXT        NOT NULL,

    FOREIGN KEY (id, passagetype) REFERENCES passages (id, passagetype)
);

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
    created_at      TIMESTAMPTZ NOT NULL
);

CREATE TABLE bookshelf_items (
    bookshelf_id    INT         NOT NULL REFERENCES bookshelves
                                    ON DELETE CASCADE,
    quest_id        INT         NOT NULL REFERENCES quests
                                    ON DELETE CASCADE,
    added_at        TIMESTAMPTZ NOT NULL,

    PRIMARY KEY (bookshelf_id, quest_id)
);

-- Bans
CREATE TABLE bans (
    id              SERIAL      PRIMARY KEY,
    user_id         INT         NOT NULL REFERENCES users
                                    ON DELETE CASCADE,
    created_at      TIMESTAMPTZ NOT NULL DEFAULT now(),
    created_by      INT         NULL REFERENCES users
                                    ON DELETE SET NULL,
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
