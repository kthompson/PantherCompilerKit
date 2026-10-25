import path from "node:path";
import { defineConfig } from "vitepress";

const repo = "https://github.com/kthompson/PantherCompilerKit";

// Pages under architecture/ link to source files and ROADMAP.md with paths
// that work on GitHub. Anything that is not a page of this site is sent to
// GitHub instead of becoming a dead link.
function linkOutsideSiteToGitHub(md: any) {
  md.core.ruler.push("panther_repo_links", (state: any) => {
    const page: string | undefined = state.env.relativePath;
    if (!page) return;
    const visit = (tokens: any[]) => {
      for (const token of tokens) {
        if (token.children) visit(token.children);
        if (token.type !== "link_open") continue;
        const href: string = token.attrGet("href") ?? "";
        if (/^([a-z]+:|#|\/)/i.test(href)) continue;
        const [target, hash] = href.split("#");
        const resolved = path.posix.normalize(
          path.posix.join(path.posix.dirname(page), target),
        );
        const isPage = !path.posix.extname(target) || target.endsWith(".md");
        if (!resolved.startsWith("../") && isPage) continue;
        const kind = target.endsWith("/") ? "tree" : "blob";
        const repoPath = path.posix.normalize(path.posix.join("docs", resolved));
        token.attrSet(
          "href",
          `${repo}/${kind}/main/${repoPath}${hash ? `#${hash}` : ""}`,
        );
      }
    };
    visit(state.tokens);
  });
}

// doccheck writes the compiler's diagnostics into expect-error blocks as
// trailing `// error: ...` comments. Mark the comment and the code it refers
// to so the theme can style them as an error.
const markErrorComments = {
  name: "panther-error-comments",
  span(this: any, node: any, line: number, col: number, _el: any, token: any) {
    const text: string = this.source.split("\n")[line - 1] ?? "";
    const at = text.indexOf("// error: ");
    if (at < 0) return;
    const content: string = token.content;
    if (col >= at || content.trimStart().startsWith("// error: "))
      this.addClassToHast(node, "error-comment");
    else if (token.content.trim()) this.addClassToHast(node, "error-code");
  },
};

export default defineConfig({
  title: "Panther Compiler Kit",
  description: "Documentation for the Panther language and compiler",
  cleanUrls: true,
  lastUpdated: true,
  srcExclude: ["README.md"],
  appearance: "dark",
  head: [
    ["link", { rel: "icon", href: "/favicon.svg" }],
    ["meta", { name: "theme-color", content: "#08080a" }],
  ],
  markdown: {
    // Panther's surface syntax is close enough to Scala for highlighting.
    languageAlias: { panther: "scala" },
    // What a block prints; shown as plain text.
    languages: [{ name: "output", scopeName: "text.output", patterns: [] }],
    config: linkOutsideSiteToGitHub,
    codeTransformers: [markErrorComments],
  },
  themeConfig: {
    nav: [
      { text: "Language", link: "/language/basics/" },
      { text: "Samples", link: "/language/samples/" },
      { text: "Architecture", link: "/architecture/pvm" },
    ],
    sidebar: {
      "/language/": [
        {
          text: "Basics",
          link: "/language/basics/",
          items: [
            { text: "Getting Started", link: "/language/basics/getting-started" },
            { text: "Variables", link: "/language/basics/variables" },
            { text: "Operators", link: "/language/basics/operators" },
            { text: "Comments", link: "/language/basics/comments" },
          ],
        },
        {
          text: "Functions",
          link: "/language/functions/",
          items: [
            { text: "Defining Functions", link: "/language/functions/defining-functions" },
            { text: "Parameters", link: "/language/functions/parameters" },
            { text: "Return Values", link: "/language/functions/return-values" },
            { text: "Higher-Order Functions", link: "/language/functions/higher-order-functions" },
          ],
        },
        {
          text: "Flow Control",
          link: "/language/flow-control/",
          items: [
            { text: "Conditional Statements", link: "/language/flow-control/conditional-statements" },
            { text: "Loops", link: "/language/flow-control/loops" },
            { text: "Pattern Matching", link: "/language/flow-control/pattern-matching" },
          ],
        },
        {
          text: "Data Types",
          link: "/language/data-types/",
          items: [
            { text: "Primitive Types", link: "/language/data-types/primitive-types" },
            { text: "Collections", link: "/language/data-types/collections" },
            { text: "Option and Result", link: "/language/data-types/option-and-result" },
            { text: "Custom Types", link: "/language/data-types/custom-types" },
            { text: "Traits and Givens", link: "/language/data-types/traits-and-givens" },
          ],
        },
        {
          text: "Samples",
          link: "/language/samples/",
          items: [
            { text: "FizzBuzz", link: "/language/samples/fizzbuzz" },
            { text: "Fibonacci", link: "/language/samples/fibonacci" },
            { text: "Expression Evaluator", link: "/language/samples/expression-evaluator" },
          ],
        },
      ],
      "/architecture/": [
        {
          text: "Architecture",
          items: [
            { text: "PVM Specification", link: "/architecture/pvm" },
            { text: "Removing Scala", link: "/architecture/scala-exit" },
          ],
        },
        {
          text: "Decision Records",
          items: [
            { text: "0001 Generic type inference", link: "/architecture/adr/0001-generic-type-inference" },
            { text: "0002 Binding this", link: "/architecture/adr/0002-binding-this" },
            { text: "0003 Equality on reference types", link: "/architecture/adr/0003-equality-on-reference-types" },
            { text: "0004 Traits and given evidence", link: "/architecture/adr/0004-traits-given-evidence-and-contextual-extensions" },
            { text: "0005 Evidence representation", link: "/architecture/adr/0005-evidence-representation" },
            { text: "0006 Conditional givens", link: "/architecture/adr/0006-conditional-givens" },
            { text: "0007 Garbage collection", link: "/architecture/adr/0007-garbage-collection" },
          ],
        },
      ],
    },
    socialLinks: [{ icon: "github", link: repo }],
    editLink: {
      pattern: `${repo}/edit/main/docs/:path`,
    },
    search: { provider: "local" },
    outline: "deep",
  },
});
