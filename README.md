The Elm/Miso model-update-view pattern is attractive on the surface, but provides a leaky abstraction and lacks composibility.

The Reflex model has good encapsulation and composibility, but can be difficult to follow because the business logic, ui logic, and html are all tangled together.

Perhaps there is some sort of hybrid solution.

The `view` needs to be powerful enough to allow the user to define custom controls. But, it should not encapsulate the *business logic*.

We want a model which describes the data relevant to the application:

    data Model = Model { .. }

Then we have messages aka actions which can be used to indicate changes to the model:

    data Action = Action { .. }

The `update` function applies an `Action` to the `Model`:

    update :: Action -> Model -> Model

The `view` creates a UI which can be used to view the `Model` and to create new `Actions` to modify the `Model`. In Miso we have a view like:

    view :: Model -> View Action

The `view` function is pure -- and so all the information needed to render the view must be provided by the `Model`.

This becomes problematic when we want to create custom controls. If we think about standard control such as `<input type="text">`, we realize that it has a bunch of internal state and behavior that we have little or no control over. The position of the caret, the handling of the arrow keys, dealing with keyboard shortcuts like `C-a` are all handled internally by the control. Those details do not leak into the `Model`, `Action` and `update`.

If we wanted to create our own `<input>` element, however, we would need to track all this information. In Miso, we would be forced to add it all to the Model, Action, and update function, even though the business logic has not really changed. Changing the `view` to use a custom `<input>` element should not require any changes to the Model/Action/update trio.

In order to create custom controls, we need a way to have local state. Additional the controls must be able to add their own locally handled event handlers. The a control consists of html, css, and javascript.

Events in the DOM are inherently asynchronous. We must add listeners, and wait for the user to do something to trigger them such as pressing a key, moving the mouse, etc.

