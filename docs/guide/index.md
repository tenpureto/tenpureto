# Getting started

Tenpureto is a language-agnostic project scaffolding tool. It helps you skip the tedious part of the project setup and
lets you focus on the actual logic.

[[toc]]

## Installing tenpureto

On **MacOS** you can install `tenpureto` with [Homebrew](https://brew.sh):

```
$ brew tap rtimush/tap
$ brew install tenpureto
```

On **Linux** you can install `tenpureto` from the binary packages available on
[BinTray](https://bintray.com/tenpureto/).

If there is no pre-built package available for your OS, you can build `tenpureto` from
[sources](https://github.com/rtimush/tenpureto/blob/master/README.md#build-from-sources).

## Scaffolding a new project

First, you need to find a suitable template. There are not so many
[open-source templates](https://github.com/topics/tenpureto-template) available yet (but you can always
[create](/guide/authoring/) your own). If your company uses `tenpureto` as a project templating solution, ask your
colleagues about the templates they use. For this guide, let's assume you want to create a `Scala` project using
[rtimush/scala-project-template](https://github.com/rtimush/scala-project-template) as a template.

Run `tenpureto create` providing the name of the template you want to use and the name of your project. It will ask you
for template features to include, refine some variable values, and after that will create a git repository with your new
project content.

<pre class="language-terminal"><code>$ <span class="white">tenpureto create --template rtimush/scala-project-template my-cool-project</span>
 <span class="green char">✓</span> 1) <span class="white">scala</span>      <span class="green">Basic template to kickstart Scala projects</span>
 <span class="green char">✓</span> 2) <span class="white">ci.travis</span>  <span class="green">Build on Travis CI</span>
 <span class="red char">𐄂</span> 3) <span class="white">ci.circle</span>  <span class="green">Build on Circle CI <span class="black">[conflict]</span></span>
Organization <span class="black">[org.organization]</span> my.organization
Project name <span class="black">[scala-project-template]</span> my-cool-project
Created /workspace/my-cool-project/.
</code></pre>

## Creating your own templates

Let's now create a very simple template for an Open Source project. Let's assume that every Open Source project needs a
README file and a LICENSE which is either Apache License 2.0 or MIT. So, we will create a template with the following
features:

- `master`
- `license.apache`
- `license.bsd`

First we will create a new git repository for the template:

```sh
$ git init open-source-project-template
$ cd open-source-project-template
```

Create a `README.md` file with the following content:

```md
# open-source-project-template

Project description
```

As you see the README file contains some placeholder text. We can tell Tenpureto that this placeholders needs to be
replaced with user-provided values by creating a `.template.yaml`:

```yaml
variables:
  Project name: open-source-project-template
  Description: Project description
features:
  - master:
      description: Basic Open Source project
```

```sh
$ git add README.md .template.yaml
$ git commit -m "Basic template"
```

Now we can create two branches for different licenses

```sh
$ git checkout -b license.apache master
```

Copy Apache License 2.0 text to `LICENSE`, and change the `features` section of `.template.yaml` (do not change the
`variables` section):

```yaml
features:
  - master:
      description: Basic Open Source project
  - license.apache:
      description: Apache License 2.0
```

Commit your changes:

```sh
$ git add LICENSE .template.yaml
$ git commit -m "Apache License 2.0"
```

Repeat the same for a BSD license branch (use `license.bsd` as a branch name).

Now you can test your template:

```sh
$ cd /tmp
$ tenpureto create --template /full/path/to/open-source-project-template cool-project
 ✓ 1) master          Basic Open Source project
 ✓ 2) license.apache  Apache License 2.0
   3) license.bsd     BSD License
Add or remove a feature:
Project name [open-source-project-template] cool-project
Description [Project description] Trying out Tenpureto
Created /tmp/cool-project/.
$ cat /tmp/cool-project/README
# cool-project

Trying out Tenpureto
```

If you want to make you template available to other people, create a public repository on GitHub (or you can of course
use any other Git hosting), and push all three branches to the remote.

For more details see the [authoring templates](/guide/authoring/) section.
