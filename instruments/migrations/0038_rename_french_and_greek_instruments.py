# -*- coding: utf-8 -*-
# Generated by Django 1.11.11 on 2018-03-23 21:06
from __future__ import unicode_literals

from django.db import migrations
from common.models import Source

def rename_sources(apps, schema_editor):
    Source = apps.get_model("common", "Source")
    Source.objects.filter(instrument_language = 'French (France)').update(instrument_language = 'French (French)')
    Source.objects.filter(instrument_language = 'French (Quebec)').update(instrument_language = 'French (Quebecois)')
    Source.objects.filter(instrument_language = 'Cypriot Greek').update(instrument_language = 'Greek (Cypriot)')

def undo_rename_sources(apps, schema_editor):
    Source = apps.get_model("common", "Source")
    Source.objects.filter(instrument_language = 'French (French)').update(instrument_language = 'French (France)')
    Source.objects.filter(instrument_language = 'French (Quebecois)').update(instrument_language = 'French (Quebec)')
    Source.objects.filter(instrument_language = 'Greek (Cypriot)').update(instrument_language = 'Cypriot Greek')


class Migration(migrations.Migration):

    dependencies = [
        ('instruments', '0037_update_hebrew_ws'),
    ]

    operations = [
        migrations.RenameModel(
            old_name='French_France_WG',
            new_name='French_French_WG',
        ),
        migrations.RenameModel(
            old_name='French_France_WS',
            new_name='French_French_WS',
        ),
        migrations.RenameModel(
            old_name='French_Quebec_WG',
            new_name='French_Quebecois_WG',
        ),
        migrations.RenameModel(
            old_name='French_Quebec_WS',
            new_name='French_Quebecois_WS',
        ),
        migrations.RenameModel(
            old_name='Cypriot_Greek_WS',
            new_name='Greek_Cypriot_WS',
        ),
        migrations.RunPython(rename_sources, undo_rename_sources)
    ]
