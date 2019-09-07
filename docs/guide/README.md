# Guide

Tenpureto is a language-agnostic project scaffolding tool. It helps you skip the tedious part of the project setup and
lets you focus on the actual logic.

[[toc]]

## Getting started

### Installing tenpureto

On **MacOS** you can install `tenpureto` with [Homebrew](https://brew.sh):

```
$ brew tap rtimush/tap
$ brew install tenpureto
```

On **Linux** you can install `tenpureto` from the binary packages available on
[BinTray](https://bintray.com/tenpureto/).

If there is no pre-built package available for your OS, you can build `tenpureto` from
[sources](https://github.com/rtimush/tenpureto/blob/master/README.md#build-from-sources).

### Scaffolding a new project

First, you need to find a suitable template. There are not so many open-source templates available yet (but you can
always [create](#creating-your-own-templates) your own). If your company uses `tenpureto` as a project templating
solution, ask your colleagues about the templates they use. For this guide, let's assume you want to create a `Scala`
project using [rtimush/scala-project-template](https://github.com/rtimush/scala-project-template) as a template.

Run `tenpureto create` providing the name of the template you want to use and the name of your project. It will ask you
for template features to include, refine some variable names, and after that will create a new git repository with your
new project content.

<pre class="language-terminal"><code>$ <span class="white">tenpureto create --template rtimush/scala-project-template my-cool-project</span>
 <span class="green char">✓</span> 1) <span class="white">scala</span>      <span class="green">Basic template to kickstart Scala projects</span>
 <span class="green char">✓</span> 2) <span class="white">ci.travis</span>  <span class="green">Build on Travis CI</span>
 <span class="red char">𐄂</span> 3) <span class="white">ci.circle</span>  <span class="green">Build on Circle CI <span class="black">[conflict]</span></span>
Organization <span class="black">[org.organization]</span> my.organization
Project name <span class="black">[scala-project-template]</span> my-cool-project
Created /workspace/my-cool-project/.
</code></pre>

## Creating your own templates

Coming soon.
