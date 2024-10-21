<template>
  <v-card rounded="xl">
    <v-container>
      <v-card-title> Apply rate of change filter </v-card-title>

      <v-form ref="myForm" validate-on="blur">
        <v-card-text>
          Select points where the rate of change is
          <v-select
            class="mt-2"
            :items="[
              'Greater than',
              'Greater than or equal to',
              'Less than',
              'Less than or equal to',
            ]"
            label="Comparison operator"
          />
          <v-text-field
            v-model.number="valueThreshold"
            label="Rate of change"
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
