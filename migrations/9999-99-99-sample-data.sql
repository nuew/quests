    INSERT INTO users (name, email, password_hash) VALUES
        ('nuew', 'nuew@nuew.net', ''),
        ('test', 'test@example.com', '');
    INSERT INTO user_slugs (slug, user_id) VALUES
        ('nuew', 1),
        ('test', 2);

    SAVEPOINT user_init;

    INSERT INTO chats (topic) VALUES
        ('Test Chat 1,000,000'),
        ('Ideas for future tests:');
    INSERT INTO chat_roles (chat_id, user_id, role) VALUES
        (1, 1, 'Owner'),
        (2, 1, 'Owner'),
        (2, 2, 'Moderator');
    INSERT INTO messages (chat_id, user_id, message) VALUES
        (1, 1, 'Hello World!');

    SAVEPOINT chat_init;

    INSERT INTO polls (system, prompt) VALUES
        ('Approval', 'Best Administrator?');
    INSERT INTO poll_choices (poll_id, user_id, choice) VALUES
        (1, 1, 'nuew'),
        (1, 2, 'Queen Administrator');
    INSERT INTO poll_ballot_choice (choice_id, user_id) VALUES
        (1, 1);

    SAVEPOINT poll_init;

    INSERT INTO quests (name, teaser, visibility, only_approved) VALUES
        ('Test Quest 1,000,000', 'A test quest.', 'Public', False);
    INSERT INTO quest_roles (quest_id, user_id, role) VALUES
        (1, 1, 'Author');
    INSERT INTO chapters (quest_id, name, position, is_appendix) VALUES
        (1, 'Test Chapter 1,000,000', 0, False),
        (1, 'Yet Another Test Chapter', 1, False),
        (1, 'The Absent Appendix', 0, True);
    INSERT INTO passages (passagetype, chapter_id, position) VALUES
        ('Chat', 1, 3),
        ('Dice', 1, 1),
        ('Poll', 1, 2),
        ('Textual', 1, 0),
        ('Textual', 2, 0);
    INSERT INTO chat_passages (id, chat_id) VALUES
        (1, 2);
    INSERT INTO dice_passages (id, best_of, quantity, sides, threshold) VALUES
        (2, 3, 4, 6, 20);
    INSERT INTO dice_rolls (passage_id, user_id, values, position) VALUES
        (2, 2, '{5, 3, 5, 6}', 0);
    INSERT INTO poll_passages (id, poll_id) VALUES
        (3, 1);
    INSERT INTO textual_passages (id, contents) VALUES
        (4, 'Hello World!'),
        (5, 'This is an example passage.');

    SAVEPOINT quest_init;

    INSERT INTO tags (name) VALUES
        ('test');
    INSERT INTO quest_tags (tag_id, quest_id) VALUES
        (1, 1);

    SAVEPOINT tag_init;

    INSERT INTO bookshelves (owner, name, description, icon, email_updates, visibility) VALUES
        (1, 'Test Bookshelf 1,000,000', 'A test bookshelf.', '!', False, 'Public');
    INSERT INTO bookshelf_items (bookshelf_id, quest_id) VALUES
        (1, 1);

    SAVEPOINT bookshelf_init;

    INSERT INTO bans (user_id, created_by, expires_at, reason) VALUES
        (2, 1, now() + '1 week', 'Test Ban 1,000,000');
    INSERT INTO quest_bans (ban_id, quest_id) VALUES
        (1, 1);
    INSERT INTO chat_bans (ban_id, chat_id) VALUES
        (1, 1);
