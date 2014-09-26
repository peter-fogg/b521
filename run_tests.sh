#!/bin/bash

racket -e "(require \"a$1-student-tests.rkt\") (test-file #:file-name \"a$1.rkt\")"
