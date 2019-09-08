# Authoring templates

Tenpureto tries to make template authoring as easy as possible. You don't need to learn any new templating language, a
template is just a regular Git repository, that you can build and test as you would usually do. Every template feature
is a branch in the template repository, and the only special thing you need to do is to add a `.template.yaml` file to
tell Tenpureto how to use the template.

## Basics

One of the big advantages of Tenpureto is that the templates are composable. This allows starting projects with a
template that is as close as possible to the desired set of features while keeping template maintenance manageable. When
you create a template think of the options you want to provide. The examples of template features you might think of
are:

- Programming languages (e.g. JavaScript and TypeScript flavors of the same template),
- Project types (e.g. a library, a pure backend service, or a service that has both a backend and a frontend),
- Service deployment alternatives (e.g. different cloud providers, Kubernetes, or serverless),
- CI services.

You will create a template branch for each of the features you want to provide, and Tenpureto will take care of merging
them. Note that some features can be built on top of others. For example, a "full-stack service" will be an extension to
the "pure backend service".

The second aspect you need to think of when you create a template is variables. Tenpureto templates don't have any
special syntax to mark the variables to be replaced. All you need is to define the variables in `.template.yaml`, and
Tenpureto will replace all occurrences. It will also take care of using the correct style of the variable value:
`template-project` will be replaced with `my-cool-service`, while `Template Project` will become `My Cool Project`.

## Template descriptor

Every branch of the template needs to have a `.template.yaml` file describing the feature this branch provides and
relations to other features and template variables. The file format is the following:

```yaml
variables:
  <variable name>: <value used in the template>
  ...
features:
  - <feature name>:
      description: <feature description>
      stability: stable | experimental | deprecated
      hidden: true | false
  - ...
conflicts:
  - <feature name>
  - ...
excludes:
  - <exclude pattern>
```

### Variables

The `variables` section defines all template variables relevant to the template feature. It is a dictionary, with
human-friendly variable names as keys, and default variable values as dictionary values. Every occurence of a default
variable value in the template (both in a file content and in a file path) will be replaced with a value provided by a
user. Tenpureto will match the style of the value when doing a replacement. In the `.template.yaml` try to use the most
natural value style. For example, the project description will probably be just a text, and for a Java package name, it
makes sense to use a dot-separated value.

### Features

In the `features` section you describe the template features provided by the template branch. The feature names must
match the Git branch names. You need to list all features provided by this template branch, including the ones coming
from the parent branches. For each feature you may provide some additional information:

- `description` — human-friendly feature description that will be shown by the `tenpureto` CLI when using the template,
- `stability` — how mature the feature is, all features that are not marked as `stable` will have a corresponding note
  in the CLI,
- `hidden` — whether the feature should be available for selection in the CLI. In some cases, it is convenient to have a
  feature branch that is not directly available for selection, but that template authors can use to share code between
  other branches.

### Conflicts

In some cases, you may want to mark features as conflicting with each other. For example, you probably don't want to
have a project built by two different CI services simultaneously. You can list the features that conflict with the
current one in the `conflicts` section.

### Excludes

Sometimes you want to have files in the template repository that shouldn't end up in generated projects. The typical
examples are README files, or a CI pipeline definition (if the template uses a different CI system than the target
projects). You can list these files in the `conflicts` section using the same pattern format as is used for
[gitignore](https://git-scm.com/docs/gitignore#_pattern_format) files.
