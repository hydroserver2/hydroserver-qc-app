<template>
  <v-form ref="form">
    <v-card>
      <v-card-title class="d-flex justify-space-between align-center"
        ><span>Add Data Points</span>
        <v-btn @click="addRow" title="Add Row" variant="outlined" rounded
          ><v-icon>mdi-plus</v-icon></v-btn
        ></v-card-title
      >

      <v-divider></v-divider>

      <v-card-text>
        <div class="mt-4">
          <v-row v-for="(point, index) of dataPoints">
            <v-col cols="1"
              ><v-badge
                class="mt-4"
                color="info"
                :content="index + 1"
                inline
              ></v-badge
            ></v-col>
            <v-col
              ><v-text-field
                v-maska="options"
                label="Datetime"
                placeholder="YYYY-MM-DD HH:MM:SS"
                hint="i.e: 2024-12-30 18:00:00"
                v-model="point[0]"
                :rules="[...required, ...dateTimeFormat]"
                clearable
              />
            </v-col>
            <v-col
              ><v-text-field
                type="number"
                label="Value"
                :rules="requiredNumber"
                v-model.number="point[1]"
            /></v-col>
            <v-col cols="1"
              ><v-btn
                class="mt-2"
                icon="mdi-close"
                variant="text"
                color="error"
                rounded
                title="Remove"
                @click="dataPoints.splice(index, 1)"
              />
            </v-col>
          </v-row>
        </div>
      </v-card-text>

      <v-divider></v-divider>

      <v-card-actions>
        <v-spacer />
        <v-btn-cancel @click="$emit('close')">Cancel</v-btn-cancel>
        <v-btn
          rounded="xl"
          variant="outlined"
          @click="onAddDataPoints"
          :disabled="!form?.isValid"
          >Add Data Points</v-btn
        >
      </v-card-actions>
    </v-card>
  </v-form>
</template>

<script setup lang="ts">
import { usePyStore } from '@/store/py'
import { onMounted, reactive, Ref } from 'vue'
import type { MaskInputOptions } from 'maska'

const { addDataPoints } = usePyStore()
import { vMaska } from 'maska/vue'
import { ref } from 'vue'
import { dateTimeFormat, required, requiredNumber } from '@/utils/rules'
import { VForm } from 'vuetify/lib/components/index.mjs'

const form = ref<InstanceType<typeof VForm>>()

const dataPoints: Ref<
  [
    datetime: string,
    value: number,
    qualifier: Partial<{ resultQualifiers: string[] }>
  ][]
> = ref([['', 0, { resultQualifiers: [] }]])
const options = reactive<MaskInputOptions>({
  mask: '####-##-## ##:##:##',
  eager: true,
})

const addRow = () => {
  dataPoints.value.push(['', 0, { resultQualifiers: [] }])
  form.value?.validate()
}

const emit = defineEmits(['close'])

const onAddDataPoints = () => {
  addDataPoints(dataPoints.value)
  emit('close')
}

onMounted(() => {
  form.value?.validate()
})
</script>

<style lang="scss" scoped>
.v-card-text {
  height: 500px;
  resize: vertical;
  overflow-y: auto;
}
</style>
