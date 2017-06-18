#!/usr/bin/env python
# TODO will this work without matplotlib 1.5.1? apparently seaborn has an issue with 1.5.3

from shmlast.last import lastal_task

def task_lastal():
        return lastal_task('query.fna', 'db.faa', translate=True)
