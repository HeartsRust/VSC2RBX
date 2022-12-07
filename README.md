# VSC2RBX
VSC2RBX is an extension that allows you to execute code from VSCode in Roblox Studio. In terms of how it works, it utilizes web sockets to communicate between the two programs. This extension is still in development, so there may be some bugs. If you find any, please report them in the issues tab.

## Installation
1. Install the extension from the VSCode Marketplace.
2. Build the plugin using the instructions below.

```bash
start build
```

3. Open Roblox Studio and go to Plugins > Manage Plugins > Open Plugins Folder.
4. Drag the built plugin into the folder that opens.

## Usage
1. If it's your first time using the extension you're gonna need to activate the extension you can activate the extension by pressing `Ctrl + Shift + P` and typing `VSCode to Roblox Activate`.
2. Once you've activated the extension you should see a new button on the bottom left of your screen titled `Execute Script`.
3. Create a tasks folder in your workspace and create a new file called `task.lua`.
4. Write some example code like:
```lua
print("Hello World!")
```

5. Press the `Execute Script` button and you should see the output in the Output tab.

## Known Issues
- The plugin will throw an error if you try to execute a script that has Compound Assignments in it (+=, -=, *=, etc.)