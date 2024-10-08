<template>
  <v-card>
    <v-card-title>Change Values</v-card-title>
    <v-card-subtitle>
      <div>
        {{ selectedData.length }} Data Point{{
          selectedData.length === 1 ? '' : 's'
        }}
        selected
      </div>
    </v-card-subtitle>

    <v-card-text>
      <div class="d-flex gap-1">
        <v-select
          label="Operation"
          :items="operators"
          v-model="selectedOperator"
          hide-details
        ></v-select>
        <v-text-field
          label="Value"
          v-model="operationValue"
          step="0.1"
          type="number"
          width="30"
          hide-details
        >
        </v-text-field>
      </div>
    </v-card-text>

    <v-card-actions>
      <v-spacer />
      <v-btn-cancel @click="$emit('close')">Cancel</v-btn-cancel>
      <v-btn rounded="xl" variant="outlined" @click="onChangeValues"
        >Change Values</v-btn
      >
    </v-card-actions>
  </v-card>
</template>

<script setup lang="ts">
import { usePyStore } from '@/store/py'
import { storeToRefs } from 'pinia'
import { useDataVisStore } from '@/store/dataVisualization'
const { selectedData } = storeToRefs(useDataVisStore())

const { changeValues, operators } = usePyStore()
const { selectedOperator, operationValue } = storeToRefs(usePyStore())

const emit = defineEmits(['close'])

const onChangeValues = () => {
  changeValues()
  emit('close')
}
</script>
