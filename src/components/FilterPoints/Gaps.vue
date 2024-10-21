<template>
  <v-card rounded="xl">
    <v-container>
      <v-card-title> Select Gaps </v-card-title>

      <v-form ref="myForm" validate-on="blur">
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

const emit = defineEmits(['filter', 'close'])
</script>
