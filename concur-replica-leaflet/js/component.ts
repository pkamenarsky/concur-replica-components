type DOM = {
  type: 'text',
  text: string
} | {
  type: 'leaf',
  attrs: { [key: string]: string | null },
  element: string
} | {
  type: 'node',
  attrs: { [key: string]: string | null },
  element: string,
  children: DOM[]
};

type Value = string | boolean | null | { [key: string]: Value };

type AttrDiff = {
  type: 'delete',
  key: string
} | {
  type: 'insert',
  key: string,
  value: Value
} | {
  type: 'diff',
  key: string,
  diff: ValueDiff[]
};

type ValueDiff = {
  type: 'replace',
  value: Value
} | {
  type: 'diff',
  diff: AttrDiff[]
};

type Diff = {
  type: 'delete',
  index: number
} | {
  type: 'insert',
  index: number,
  dom: DOM
} | {
  type: 'diff',
  index: number,
  adiff: AttrDiff[],
  diff: Diff[]
} | {
  type: 'replace_text',
  index: number,
  text: string
};

type SerializedEvent = {
  eventType: string,
  event: any,
  element: Node
};

type Callbacks = {
  patchChild(domNode: Node, diff: Diff): void,
  patchAttribute(domNode: Node, diff: AttrDiff): void
};

type ComponentCallbacks = (sendEvent: (event: SerializedEvent) => void) => Callbacks;

const registerComponent: (name: string, callbacks: ComponentCallbacks) => void = (window as any)['registerComponent'];

registerComponent("leaflet", (sendEvent) => {
  return {
    patchChild: (domNode, diff) => {
      console.log(domNode, diff);
    },
    patchAttribute: (domNode, diff) => {
      console.log(domNode, diff);
    }
  };
});
