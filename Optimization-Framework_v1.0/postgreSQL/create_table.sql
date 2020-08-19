CREATE SEQUENCE "tasks_ID_seq"
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

CREATE TABLE tasks (
    "ID" integer DEFAULT nextval('"tasks_ID_seq"'::regclass) NOT NULL,
    name text,
    type text,
    run integer,
    "userName" character varying(300) DEFAULT ''::character varying,
    "workingDirectory" text DEFAULT ''::text,
    "cmdLine" text,
    "runTimeFactor" real DEFAULT 1,
    state character varying(50) DEFAULT 'free'::character varying,
    deadline time without time zone,
    "clientID" character varying(20) DEFAULT ''::character varying,
    "clientIP" character varying(30) DEFAULT ''::character varying,
    "failureCounter" smallint DEFAULT 0,
    "startTime" timestamp with time zone,
    "finishTime" timestamp with time zone DEFAULT '2015-02-11 00:00:00+01'::timestamp with time zone,
    success boolean DEFAULT false,
    output text DEFAULT ''::text,
    "errorName" character varying(100) DEFAULT ''::character varying,
    "errorValue" character varying(50) DEFAULT '0'::character varying,
    "errorName0" character varying DEFAULT ''::character varying,
    "errorValue0" character varying DEFAULT ''::character varying,
    "errorName1" character varying DEFAULT ''::character varying,
    "errorValue1" character varying DEFAULT ''::character varying,
    "errorName2" character varying DEFAULT ''::character varying,
    "errorValue2" character varying DEFAULT ''::character varying,
    "optimizationID" character varying DEFAULT ''::character varying,
    priority double precision DEFAULT 0,
    "deadlineCounter" smallint DEFAULT 0,
    "lastPing" timestamp with time zone DEFAULT '2000-01-01 00:00:00+01'::timestamp with time zone,
    "evaluationBinary" character varying DEFAULT ''::character varying
);