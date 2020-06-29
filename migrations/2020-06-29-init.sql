CREATE TYPE visibility AS ENUM ('Public', 'Unlisted', 'Private');
CREATE TYPE quest_role AS ENUM ('Author', 'Participant', 'Guest');
CREATE TYPE chat_role AS ENUM ('Moderator', 'Voiced');

-- Users
CREATE TABLE users (
    id              SERIAL      PRIMARY KEY,
    name            TEXT        NOT NULL,
    email           TEXT        NOT NULL,
    avatar          TEXT        NULL,
    created_at      TIMESTAMPTZ NOT NULL DEFAULT now(),

    biography       TEXT        NOT NULL DEFAULT '',
    location        TEXT        NOT NULL DEFAULT '',
    pronouns        TEXT        NOT NULL DEFAULT '',
    website         TEXT        NOT NULL DEFAULT ''
);

CREATE TABLE user_slugs (
    user_id         INTEGER     NOT NULL REFERENCES users
                                    ON DELETE CASCADE,
    slug            TEXT        NOT NULL PRIMARY KEY
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

-- Quests
CREATE TABLE quests (
    id              SERIAL      PRIMARY KEY,
    name            TEXT        NOT NULL,
    teaser          TEXT        NOT NULL,
    banner          TEXT        NULL,
    visibility      VISIBILITY NOT NULL,
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
    chapter_id      INT         NOT NULL REFERENCES chapters
                                    ON DELETE CASCADE,
    position        INT         NOT NULL,
    created_at      TIMESTAMPTZ NOT NULL DEFAULT now(),
    last_updated    TIMESTAMPTZ NOT NULL,
    UNIQUE (chapter_id, position)
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

-- Chats
CREATE TABLE chats (
    id              SERIAL      PRIMARY KEY,
    topic           TEXT        NOT NULL
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
