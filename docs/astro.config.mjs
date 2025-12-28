// @ts-check
import { defineConfig } from "astro/config";
import starlight from "@astrojs/starlight";

// https://astro.build/config
export default defineConfig({
  integrations: [
    starlight({
      title: "Panther Compiler Kit",
      social: [
        {
          icon: "github",
          label: "GitHub",
          href: "https://github.com/kthompson/PantherCompilerKit",
        },
      ],
      sidebar: [
        {
          label: "Start Here",
          items: [],
        },
        {
          label: "Basics",
          autogenerate: { directory: "basics" },
        },
        {
          label: "Functions",
          autogenerate: { directory: "functions" },
        },
        {
          label: "Flow Control",
          autogenerate: { directory: "flow-control" },
        },
        {
          label: "Data Types",
          autogenerate: { directory: "data-types" },
        },
        {
          label: "Reference",
          autogenerate: { directory: "reference" },
        },
      ],
    }),
  ],
});
