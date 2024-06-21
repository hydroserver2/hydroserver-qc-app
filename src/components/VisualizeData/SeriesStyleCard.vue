<template>
  <v-card rounded="xl">
    <v-container>
      <v-card-title>Select series style</v-card-title>

      <v-form @submit.prevent="onSubmit" ref="myForm" validate-on="blur">
        <v-card-text>
          <v-select
            :items="lineStyles"
            label="Select Line Style"
            v-model="selectedLineStyle"
          />

          <v-select
            :items="symbols"
            label="Select Symbol"
            v-model="selectedSymbol"
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

defineProps({ datastreamName: { type: String, required: true } })
const emit = defineEmits(['submit', 'close'])

const selectedLineStyle = ref(undefined)
const selectedSymbol = ref(undefined)

const lineStyles = ['solid', 'dashed', 'dotted', 'none']
const symbols = [
  'circle',
  'rect',
  'roundRect',
  'triangle',
  'diamond',
  'pin',
  'arrow',
  'none',
]

function onSubmit() {
  emit('submit', {
    lineStyle: selectedLineStyle.value,
    symbol: selectedSymbol.value,
  })
  emit('close')
}
</script>
