import '@/styles/global.scss'

import { createApp } from 'vue'
import store from '@/store'
import App from './App.vue'
import router from './router/router'
import vuetify from '@/plugins/vuetify'
import { _Window } from './types'

const app = createApp(App)

app.use(store)
app.use(router)
app.use(vuetify)
app.mount('#app')

export default app
