CREATE EXTENSION "uuid-ossp";
CREATE TABLE public.users (
    id          UUID    DEFAULT     uuid_generate_v4(),
    username    TEXT    NOT NULL    UNIQUE,
    email       TEXT    NOT NULL    UNIQUE,
    password    TEXT    NOT NULL,
    PRIMARY KEY (id)
);
