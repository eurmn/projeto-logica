import { defineConfig, presetIcons, presetWind, transformerDirectives, transformerVariantGroup } from 'unocss';

export default defineConfig({
  presets: [
    presetWind(),
    presetIcons(),
  ],
  transformers: [
    transformerDirectives(),
    transformerVariantGroup()
  ],
  theme: {
    fontFamily: {
      'sans': 'InterVariable',
      'title': 'Familjen GroteskVariable',
    },
  }
});