<template>
  <v-card>
    <v-card-title>Change Values</v-card-title>
    <v-card-subtitle class="mb-4">
      <div>
        {{ selectedData.length }} Data Point{{
          selectedData.length === 1 ? '' : 's'
        }}
        selected
      </div>
    </v-card-subtitle>

    <v-divider></v-divider>

    <v-card-text>
      <div class="d-flex flex-column gap-2">
        <div>
          <v-label class="mb-2 d-block"
            >Operation: <b>{{ operators[selectedOperator] }}</b></v-label
          >
          <v-btn-toggle
            v-model="selectedOperator"
            variant="outlined"
            color="primary"
            mandatory
            divided
          >
            <v-btn title="Add">
              <v-icon color="green">mdi-plus</v-icon>
            </v-btn>

            <v-btn title="Subtract">
              <v-icon color="red">mdi-minus</v-icon>
            </v-btn>

            <v-btn title="Multiply">
              <v-icon color="blue">mdi-multiplication</v-icon>
            </v-btn>

            <v-btn title="Divide">
              <v-icon color="orange">mdi-division</v-icon>
            </v-btn>

            <v-btn title="Assign">
              <v-icon>mdi-equal</v-icon>
            </v-btn>
          </v-btn-toggle>
        </div>

        <v-text-field
          label="Value"
          v-model="operationValue"
          step="0.1"
          type="number"
          block
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
import { Operator, usePyStore } from '@/store/py'
import { storeToRefs } from 'pinia'
import { useDataVisStore } from '@/store/dataVisualization'
import { useEChartsStore } from '@/store/echarts'
import { EnumEditOperations } from '@/types'

const { updateVisualization } = useEChartsStore()
const { selectedSeries, brushSelections } = storeToRefs(useEChartsStore())
const { selectedData } = storeToRefs(useDataVisStore())
const { operators } = usePyStore()
const { selectedOperator, operationValue } = storeToRefs(usePyStore())

const emit = defineEmits(['close'])

const onChangeValues = async () => {
  if (!selectedData.value.length) {
    return
  }

  const operator = Operator[operators[selectedOperator.value] as Operator]

  const index = selectedData.value.map(
    (point: { date: Date; value: number; index: number }) =>
      selectedSeries.value.data.dataFrame.get_index_at(point.index)
  )

  await selectedSeries.value.data.dispatch(
    EnumEditOperations.CHANGE_VALUES,
    index,
    operator,
    operationValue.value
  )
  brushSelections.value = []
  selectedData.value = []
  updateVisualization()

  emit('close')
}
</script>
