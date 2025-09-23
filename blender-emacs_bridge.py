import threading
import sys
import json
import os
import subprocess
import bpy

from typing import TypedDict, NotRequired, cast

print('Bridge script starting...', flush=True)

# === CONFIGURATION ===
default_addon: str = ""  # Initialize as empty string
if "--" in sys.argv:
    idx = sys.argv.index("--")
    if len(sys.argv) > idx + 1:
        default_addon = sys.argv[idx + 1]
        print(f"Default addon set from command line: {default_addon}", flush=True)
# ======================

class Command(TypedDict):
    cmd: str
    path: str
    external: NotRequired[bool]
    python_env: NotRequired[str]
    addon: str
    name: str

# Try to enable the add-on only if we have a default addon
if default_addon:
    try:
        bpy.ops.preferences.addon_enable(module=default_addon)
        print(f"Add-on '{default_addon}' enabled.", flush=True)
    except Exception as e:
        print(f"Failed to enable add-on '{default_addon}': {e}", flush=True)
else:
    print("No default addon specified - skipping addon enable", flush=True)

def run_external_python(script_path: str, python_env: str):
    """Run a Python script in an external Python environment."""
    try:
        print(f"Running {script_path} in external Python: {python_env}", flush=True)

        result = subprocess.run([
            python_env, script_path
        ], capture_output=True, text=True, timeout=30)

        if result.stdout:
            print(f"External output: {result.stdout}", flush=True)
        if result.stderr:
            print(f"External stderr: {result.stderr}", flush=True)
        if result.returncode != 0:
            print(f"External script exited with code: {result.returncode}", flush=True)

    except subprocess.TimeoutExpired:
        print("External script timed out after 30 seconds", flush=True)
    except FileNotFoundError:
        print(f"Python executable not found: {python_env}", flush=True)
    except Exception as e:
        print(f"Error running external script: {e}", flush=True)

def reload_addon_modules(addon_name: str) -> bool:
    """Reload all modules related to an add-on."""
    try:
        import importlib

        # Find and reload all modules related to the add-on
        modules_to_reload: list[str] = []
        for name in list(sys.modules.keys()):
            # Match add-on modules
            if name == addon_name or name.startswith(addon_name + "."):
                modules_to_reload.append(name)

        print(f"Found {len(modules_to_reload)} modules to reload: {modules_to_reload}", flush=True)

        # Reload in reverse order (children first, then parents)
        for module_name in reversed(modules_to_reload):
            if module_name in sys.modules:
                try:
                    _ = importlib.reload(sys.modules[module_name])
                    print(f"Reloaded module: {module_name}", flush=True)
                except Exception as e:
                    print(f"Failed to reload {module_name}: {e}", flush=True)

        return True
    except Exception as e:
        print(f"Error in module reload: {e}", flush=True)
        return False

def emacs_bridge():
    """Main bridge loop handling commands from Emacs."""
    global default_addon
    print("Blender Emacs bridge ready", flush=True)

    while True:
        try:
            line: str = sys.stdin.readline()
            if not line:
                break

            cmd: Command = cast(Command, json.loads(line.strip()))
            command = cmd.get("cmd")

            if command == "run":
                path = cmd.get("path")
                external = cmd.get("external", False)
                python_env = cmd.get("python_env")

                if not os.path.exists(path):
                    print(f"Error: File not found - {path}", flush=True)
                    continue

                if external and python_env:
                    # Run in external Python
                    run_external_python(path, python_env)
                else:
                    # Run in Blender's Python
                    try:
                        with open(path, 'r', encoding='utf-8') as f:
                            code = f.read()
                        exec(compile(code, path, 'exec'), {'__name__': '__main__'})
                        print(f"Executed: {path}", flush=True)
                    except Exception as e:
                        print(f"Error in {path}: {e}", flush=True)

            elif command == "reload_addon":
                addon_name = cmd.get("addon", default_addon)
                if not addon_name:
                    print("Error: No addon specified for reload and no default addon set", flush=True)
                    continue

                try:
                    # Disable add-on first
                    bpy.ops.preferences.addon_disable(module=addon_name)
                    print(f"Add-on '{addon_name}' disabled.", flush=True)

                    # Reload modules
                    if reload_addon_modules(addon_name):
                        # Re-enable add-on
                        bpy.ops.preferences.addon_enable(module=addon_name)
                        print(f"Add-on '{addon_name}' fully reloaded.", flush=True)
                    else:
                        print(f"Module reload failed for '{addon_name}'", flush=True)

                except Exception as e:
                    print(f"Error reloading add-on '{addon_name}': {e}", flush=True)

            elif command == "set_addon":
                new_name = cmd.get("name")
                if new_name:
                    default_addon = new_name
                    print(f"Active addon set to: {default_addon}", flush=True)
                else:
                    print("Error: No addon name provided for set_addon command", flush=True)

            else:
                print(f"Unknown command: {command}", flush=True)

        except json.JSONDecodeError as e:
            print(f"JSON decode error: {e}", flush=True)
        except Exception as e:
            print(f"Bridge error: {e}", flush=True)

# Launch the bridge in a separate thread
threading.Thread(target=emacs_bridge, daemon=True).start()
