import { createApp } from 'vue'
import './style.css'
import App from './App.vue'
import { _Window } from './types'
import { createPinia } from 'pinia'
import vuetify from '@/plugins/vuetify'

// @ts-ignore
;(window as _Window).someFunction = (message: string) => {
  alert(message)
}

const pinia = createPinia()
const app = createApp(App)

app.use(pinia)
app.use(vuetify)
app.mount('#app')
