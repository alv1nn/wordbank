# -*- coding: utf-8 -*-
# Generated by Django 1.11.11 on 2018-03-27 17:22
from __future__ import unicode_literals

from django.db import migrations, models


class Migration(migrations.Migration):

    dependencies = [
        ('common', '0014_source_license'),
    ]

    operations = [
        migrations.AddField(
            model_name='instrument',
            name='unilemma_coverage',
            field=models.DecimalField(decimal_places=2, max_digits=3, null=True),
        ),
    ]
