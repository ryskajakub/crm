--
-- PostgreSQL database dump
--

-- Dumped from database version 13.4
-- Dumped by pg_dump version 13.4

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: upkeep_form_jobs; Type: TABLE; Schema: public; Owner: haskell
--

CREATE TABLE public.upkeep_form_jobs (
    upkeep_id integer NOT NULL,
    date_ timestamp without time zone NOT NULL,
    travel_there_from time without time zone NOT NULL,
    travel_there_to time without time zone NOT NULL,
    work_to time without time zone NOT NULL,
    work_from time without time zone NOT NULL,
    travel_back_from time without time zone NOT NULL,
    travel_back_to time without time zone NOT NULL,
    note text NOT NULL
);


ALTER TABLE public.upkeep_form_jobs OWNER TO haskell;

--
-- Name: upkeep_form_mails; Type: TABLE; Schema: public; Owner: haskell
--

CREATE TABLE public.upkeep_form_mails (
    upkeep_id integer NOT NULL,
    email character varying(500) NOT NULL,
    sent_at timestamp without time zone NOT NULL
);


ALTER TABLE public.upkeep_form_mails OWNER TO haskell;

--
-- Name: upkeep_form_parts; Type: TABLE; Schema: public; Owner: haskell
--

CREATE TABLE public.upkeep_form_parts (
    upkeep_id integer NOT NULL,
    number text NOT NULL,
    name text NOT NULL,
    quantity text NOT NULL,
    machine_id integer
);


ALTER TABLE public.upkeep_form_parts OWNER TO haskell;

--
-- Name: upkeep_form_signatures; Type: TABLE; Schema: public; Owner: haskell
--

CREATE TABLE public.upkeep_form_signatures (
    upkeep_id integer NOT NULL,
    theirs text,
    ours text
);


ALTER TABLE public.upkeep_form_signatures OWNER TO haskell;

--
-- Name: upkeep_forms; Type: TABLE; Schema: public; Owner: haskell
--

CREATE TABLE public.upkeep_forms (
    transport integer,
    upkeep_id integer NOT NULL,
    warranty boolean NOT NULL,
    no_faults boolean NOT NULL
);


ALTER TABLE public.upkeep_forms OWNER TO haskell;

--
-- PostgreSQL database dump complete
--

