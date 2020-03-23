module.exports = {
  title: "Tenpureto",
  description: "Simple and flexible project templates",
  themeConfig: {
    nav: [
      { text: "Home", link: "/" },
      { text: "Guide", link: "/guide/" },
    ],
    sidebar: ["/", "/guide/", "/guide/authoring/"],
    repo: "tenpureto/tenpureto",
    docsDir: "docs",
    editLinks: true,
  },
  plugins: {
    sitemap: {
      hostname: "https://tenpureto.org",
      exclude: ["/404.html"],
    },
  },
};
