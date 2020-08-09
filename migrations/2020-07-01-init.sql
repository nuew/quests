CREATE TYPE chat_role AS ENUM ('Owner', 'Moderator', 'Voiced');
CREATE TYPE electionsys AS ENUM ('Approval', 'Borda', 'FPTP', 'Schulze', 'STV');
CREATE TYPE passagetype AS ENUM ('Chat', 'Dice', 'Poll', 'Textual');
CREATE TYPE quest_role AS ENUM ('Author', 'Participant', 'Guest');
CREATE TYPE shelf_role AS ENUM ('Editor', 'Visitor');
CREATE TYPE visibility AS ENUM ('Public', 'Unlisted', 'Private');

CREATE TYPE report_type AS ENUM (
    'User',
    'Bookshelf',
    'Quest',
    'Chapter',
    'Passage',
    'Poll',
    'Choice',
    'Chat',
    'Message',
    'Tag'
);

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

CREATE TABLE user_followers (
    following_id    INTEGER     NOT NULL REFERENCES users
                                    ON DELETE CASCADE,
    followed_id     INTEGER     NOT NULL REFERENCES users
                                    ON DELETE CASCADE,
    created_at      TIMESTAMPTZ NOT NULL DEFAULT now(),

    PRIMARY KEY (following_id, followed_id)
);

CREATE VIEW users_short AS
    SELECT u.id, u.name, l.slug, u.avatar, l.created_at, s.last_active
    FROM users u
    INNER JOIN user_slugs l ON u.id = l.user_id
    LEFT JOIN sessions s ON u.id = s.user_id;

CREATE VIEW users_long AS
    SELECT u.id, u.name, l.slug, u.email, u.avatar, u.created_at, s.last_active,
           u.password_hash, u.biography, u.location, u.pronouns, u.website
    FROM users u
    INNER JOIN user_slugs l ON u.id = l.user_id
    LEFT JOIN sessions s ON u.id = s.user_id;

CREATE VIEW active_sessions AS
    SELECT *
    FROM sessions
    WHERE expires_at < now();

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

CREATE VIEW chats_long AS
    SELECT c.id, c.topic, c.open, c.voiced_only, m.last_updated
    FROM chats c
    LEFT JOIN messages m ON m.chat_id = c.id;

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
    created_at      TIMESTAMPTZ NOT NULL DEFAULT now(),
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

CREATE VIEW orphaned_passages AS
    SELECT p.*
    FROM passages p
    LEFT JOIN chat_passages c ON p.id = c.id
    LEFT JOIN dice_passages d ON p.id = d.id
    LEFT JOIN poll_passages v ON p.id = v.id
    LEFT JOIN textual_passages t ON p.id = t.id
    WHERE
        c.id IS NULL AND
        d.id IS NULL AND
        v.id IS NULL AND
        t.id IS NULL;

CREATE VIEW quest_passages AS
    SELECT q.id as quest_id, p.*
    FROM passages p
    INNER JOIN chapters c ON p.chapter_id = c.id
    INNER JOIN quests q ON c.quest_id = q.id;

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

-- Reports
CREATE TABLE reports (
    id              SERIAL      PRIMARY KEY,
    report_type     REPORT_TYPE NOT NULL,
    created_at      TIMESTAMPTZ NOT NULL DEFAULT now(),
    created_by      INT         NULL REFERENCES users
                                    ON DELETE SET NULL,
    resolved_at     TIMESTAMPTZ NULL,
    resolved_by     INT         NULL REFERENCES users
                                    ON DELETE CASCADE,
    reason          TEXT        NOT NULL,

    CHECK ((resolved_at IS NULL AND resolved_by IS NULL) OR
           (resolved_at IS NOT NULL AND resolved_by IS NOT NULL)),
    UNIQUE (id, report_type)
);

CREATE TABLE user_reports (
    id              INT         PRIMARY KEY,
    report_type     REPORT_TYPE NOT NULL DEFAULT 'User'
                                    CHECK (report_type = 'User'),
    user_id         INT         NOT NULL REFERENCES users
                                    ON DELETE CASCADE,

    FOREIGN KEY (id, report_type) REFERENCES reports (id, report_type)
        ON DELETE CASCADE
);

CREATE TABLE bookshelf_reports (
    id              INT         PRIMARY KEY,
    report_type     REPORT_TYPE NOT NULL DEFAULT 'Bookshelf'
                                    CHECK (report_type = 'Bookshelf'),
    bookshelf_id    INT         NOT NULL REFERENCES bookshelves
                                    ON DELETE CASCADE,

    FOREIGN KEY (id, report_type) REFERENCES reports (id, report_type)
        ON DELETE CASCADE
);

CREATE TABLE quest_reports (
    id              INT         PRIMARY KEY,
    report_type     REPORT_TYPE NOT NULL DEFAULT 'Quest'
                                    CHECK (report_type = 'Quest'),
    quest_id        INT         NOT NULL REFERENCES quests
                                    ON DELETE CASCADE,

    FOREIGN KEY (id, report_type) REFERENCES reports (id, report_type)
        ON DELETE CASCADE
);

CREATE TABLE chapter_reports (
    id              INT         PRIMARY KEY,
    report_type     REPORT_TYPE NOT NULL DEFAULT 'Chapter'
                                    CHECK (report_type = 'Chapter'),
    chapter_id      INT         NOT NULL REFERENCES chapters
                                    ON DELETE CASCADE,

    FOREIGN KEY (id, report_type) REFERENCES reports (id, report_type)
        ON DELETE CASCADE
);

CREATE TABLE passage_reports (
    id              INT         PRIMARY KEY,
    report_type     REPORT_TYPE NOT NULL DEFAULT 'Passage'
                                    CHECK (report_type = 'Passage'),
    passage_id      INT         NOT NULL REFERENCES passages
                                    ON DELETE CASCADE,

    FOREIGN KEY (id, report_type) REFERENCES reports (id, report_type)
        ON DELETE CASCADE
);

CREATE TABLE poll_reports (
    id              INT         PRIMARY KEY,
    report_type     REPORT_TYPE NOT NULL DEFAULT 'Poll'
                                    CHECK (report_type = 'Poll'),
    poll_id         INT         NOT NULL REFERENCES polls
                                    ON DELETE CASCADE,

    FOREIGN KEY (id, report_type) REFERENCES reports (id, report_type)
        ON DELETE CASCADE
);

CREATE TABLE choice_reports (
    id              INT         PRIMARY KEY,
    report_type     REPORT_TYPE NOT NULL DEFAULT 'Choice'
                                    CHECK (report_type = 'Choice'),
    choice_id       INT         NOT NULL REFERENCES poll_choices
                                    ON DELETE CASCADE,

    FOREIGN KEY (id, report_type) REFERENCES reports (id, report_type)
        ON DELETE CASCADE
);

CREATE TABLE chat_reports (
    id              INT         PRIMARY KEY,
    report_type     REPORT_TYPE NOT NULL DEFAULT 'Choice'
                                    CHECK (report_type = 'Choice'),
    chat_id         INT         NOT NULL REFERENCES poll_choices
                                    ON DELETE CASCADE,

    FOREIGN KEY (id, report_type) REFERENCES reports (id, report_type)
        ON DELETE CASCADE
);

CREATE TABLE message_reports (
    id              INT         PRIMARY KEY,
    report_type     REPORT_TYPE NOT NULL DEFAULT 'Message'
                                    CHECK (report_type = 'Message'),
    message_id      INT         NOT NULL REFERENCES messages
                                    ON DELETE CASCADE,

    FOREIGN KEY (id, report_type) REFERENCES reports (id, report_type)
        ON DELETE CASCADE
);

CREATE TABLE tag_reports (
    id              INT         PRIMARY KEY,
    report_type     REPORT_TYPE NOT NULL DEFAULT 'Tag'
                                    CHECK (report_type = 'Tag'),
    tag_id          INT         NOT NULL REFERENCES tags
                                    ON DELETE CASCADE,

    FOREIGN KEY (id, report_type) REFERENCES reports (id, report_type)
        ON DELETE CASCADE
);

CREATE VIEW absorbed_reports AS
    SELECT r.*, u.user_id, b.bookshelf_id, q.quest_id, c.chapter_id,
           p.passage_id, v.poll_id, o.choice_id, t.chat_id, m.message_id,
           g.tag_id
    FROM reports r
    LEFT JOIN user_reports u ON r.id = u.id
    LEFT JOIN bookshelf_reports b ON r.id = b.id
    LEFT JOIN quest_reports q ON r.id = q.id
    LEFT JOIN chapter_reports c ON r.id = c.id
    LEFT JOIN passage_reports p ON r.id = p.id
    LEFT JOIN poll_reports v ON r.id = v.id
    LEFT JOIN choice_reports o ON r.id = o.id
    LEFT JOIN chat_reports t ON r.id = t.id
    LEFT JOIN message_reports m ON r.id = m.id
    LEFT JOIN tag_reports g ON r.id = g.id
    WHERE
        u.id IS NOT NULL OR
        b.id IS NOT NULL OR
        q.id IS NOT NULL OR
        c.id IS NOT NULL OR
        p.id IS NOT NULL OR
        v.id IS NOT NULL OR
        o.id IS NOT NULL OR
        t.id IS NOT NULL OR
        m.id IS NOT NULL OR
        g.id IS NOT NULL;

CREATE VIEW orphaned_reports AS
    SELECT r.*
    FROM reports r
    LEFT JOIN user_reports u ON r.id = u.id
    LEFT JOIN bookshelf_reports b ON r.id = b.id
    LEFT JOIN quest_reports q ON r.id = q.id
    LEFT JOIN chapter_reports c ON r.id = c.id
    LEFT JOIN passage_reports p ON r.id = p.id
    LEFT JOIN poll_reports v ON r.id = v.id
    LEFT JOIN choice_reports o ON r.id = o.id
    LEFT JOIN chat_reports t ON r.id = t.id
    LEFT JOIN message_reports m ON r.id = m.id
    LEFT JOIN tag_reports g ON r.id = g.id
    WHERE
        u.id IS NULL AND
        b.id IS NULL AND
        q.id IS NULL AND
        c.id IS NULL AND
        p.id IS NULL AND
        v.id IS NULL AND
        o.id IS NULL AND
        t.id IS NULL AND
        m.id IS NULL AND
        g.id IS NULL;
