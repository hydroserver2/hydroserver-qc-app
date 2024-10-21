<template>
  <v-card rounded="xl">
    <v-container>
      <v-card-title> Filter by persistent values </v-card-title>

      <v-form ref="myForm" validate-on="blur">
        <v-card-text>
          Select points where the values are the same x times in a row

          <v-text-field
            class="mt-2"
            v-model.number="valueThreshold"
            label="x"
            type="number"
            clearable
          />
        </v-card-text>

        <v-card-actions>
          <v-spacer />
          <v-btn-cancel @click="$emit('close')">Cancel</v-btn-cancel>
          <v-btn rounded="xl" variant="outlined" type="submit">Filter</v-btn>
        </v-card-actions>
      </v-form>
    </v-container>
  </v-card>
</template>

<script setup lang="ts">
import { ref } from 'vue'
import { VForm } from 'vuetify/components'

const valueThreshold = ref<number | null>(null)

const emit = defineEmits(['filter', 'close'])
</script>
