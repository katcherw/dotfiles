-- Pull in the wezterm API
local wezterm = require 'wezterm'
local act = wezterm.action

local config = wezterm.config_builder()

config.font = wezterm.font('Inconsolata')
config.font_size = 12

config.font_rules = {
{
    intensity = "Bold",
    font = wezterm.font_with_fallback{
	    family = "Inconsolata",
	    weight = "Regular",
    },
},
}

config.color_scheme = 'tokyonight'

local rename_tab = act.PromptInputLine {
  description = 'Rename tab',
  action = wezterm.action_callback(function(window, _, line)
    if line then
      window:active_tab():set_title(line)
    end
  end),
}

config.keys = {
  --{ key = '%', mods = 'CTRL|SHIFT', action = act.SplitHorizontal { domain = 'CurrentPaneDomain' } },
  --{ key = '5', mods = 'CTRL|SHIFT', action = act.SplitHorizontal { domain = 'CurrentPaneDomain' } },
  --{ key = '"', mods = 'CTRL|SHIFT', action = act.SplitVertical { domain = 'CurrentPaneDomain' } },
  --{ key = "'", mods = 'CTRL|SHIFT', action = act.SplitVertical { domain = 'CurrentPaneDomain' } },
  --{ key = 'h', mods = 'CTRL|SHIFT', action = act.ActivatePaneDirection 'Left' },
  --{ key = 'j', mods = 'CTRL|SHIFT', action = act.ActivatePaneDirection 'Down' },
  --{ key = 'k', mods = 'CTRL|SHIFT', action = act.ActivatePaneDirection 'Up' },
  --{ key = 'l', mods = 'CTRL|SHIFT', action = act.ActivatePaneDirection 'Right' },
  { key = ',', mods = 'CTRL|SHIFT', action = rename_tab },
  { key = '<', mods = 'CTRL|SHIFT', action = rename_tab },
}

config.mouse_bindings = {
	{
		event = { Down = { streak = 1, button = "Right" } },
		mods = "NONE",
		action = wezterm.action_callback(function(window, pane)
			local has_selection = window:get_selection_text_for_pane(pane) ~= ""
			if has_selection then
				window:perform_action(act.CopyTo("ClipboardAndPrimarySelection"), pane)
				window:perform_action(act.ClearSelection, pane)
			else
				window:perform_action(act.PasteFrom("Clipboard"), pane)
			end
		end),
	},
}

-- and finally, return the configuration to wezterm
return config


