--========================================================================
-- nvim initialization file. 
--
-- Prefer keeping this in one file because it's easier to move from 
-- device to device.
--========================================================================

-- ================= Initialize package manager ================= 

local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

--- ================= Load packages =================

local plugins = {
  -- Color scheme
  {
    "folke/tokyonight.nvim",
    --'liuchengxu/space-vim-dark',
    lazy = false, -- make sure we load this during startup if it is your main colorscheme
    priority = 1000, -- make sure to load this before all the other start plugins
  },

  -- Figures out spaces/tabs by reading existing file
  {
    'NMAC427/guess-indent.nvim',
    opts = {
      auto_cmd = true,  -- Set to false to disable automatic execution
      filetype_exclude = {  -- A list of filetypes for which the auto command gets disabled
        "netrw",
        "tutor",
      },
      buftype_exclude = {  -- A list of buffer types for which the auto command gets disabled
        "help",
        "nofile",
        "terminal",
        "prompt",
      },
    },
  },

  -- nicer status line
  {
    'nvim-lualine/lualine.nvim',
    opts = {
      --theme = 'zenbones',
      theme = 'tokyonight',
      icons_enabled = true,
      --component_separators = '|',
      --section_separators = '',
    },
  },

  -- LSP support. Use lsp-zero for easy configuration
  {
    'VonHeikemen/lsp-zero.nvim', 
    branch = 'v3.x',
    lazy = true,
    config = false,
  },
  {
    'neovim/nvim-lspconfig',
    dependencies = {'hrsh7th/cmp-nvim-lsp'},
  },
  {
    'hrsh7th/nvim-cmp',
    dependencies = {
      {'L3MON4D3/LuaSnip'},
    }
  },


  --[[
  {'simrat39/rust-tools.nvim'},
  {
    "nvim-neo-tree/neo-tree.nvim",
    branch = "v2.x",
    dependencies = { 
      "nvim-lua/plenary.nvim",
      "nvim-tree/nvim-web-devicons", -- not strictly required, but recommended
      "MunifTanjim/nui.nvim",
    }
  } ]]

  -- Displays key mappings
  {
    "folke/which-key.nvim",
    config = function()
      vim.o.timeout = true
      vim.o.timeoutlen = 300
      require("which-key").setup({
      })
    end,
  }, 

  -- Completion framework:
  {'hrsh7th/nvim-cmp'},

  -- Useful completion sources:
  --{'hrsh7th/cmp-nvim-lsp'},
  --{'hrsh7th/cmp-nvim-lua'},
  --{'hrsh7th/cmp-nvim-lsp-signature-help'},
  --{'hrsh7th/cmp-vsnip'},
  --{'hrsh7th/cmp-path'},                              
  --{'hrsh7th/cmp-buffer'},                            
  --{'hrsh7th/vim-vsnip'},     

  -- fuzzy finder
  {
    'nvim-telescope/telescope.nvim',
     dependencies = { {'nvim-lua/plenary.nvim'} },
     opts = {
       defaults = {
         layout_strategy = 'vertical',
       }
     }
  }, 
  
  -- neotree file explorer
  {
    "nvim-neo-tree/neo-tree.nvim",
    branch = "v2.x",
    dependencies = { 
      "nvim-lua/plenary.nvim",
      "nvim-tree/nvim-web-devicons", -- not strictly required, but recommended
      "MunifTanjim/nui.nvim",
    },
  },

  {
    "folke/zen-mode.nvim",
    opts = {
      plugins = {
        ruler = true,
        showcmd = true,
      }
    }
  },

  --[[
  {
    'dhananjaylatkar/cscope_maps.nvim',
    dependencies = {"folke/which-key.nvim", 'nvim-telescope/telescope.nvim', 'nvim-tree/nvim-web-devicons'},
    commit = "fe9996c",
    opts = {
      disable_maps = false,
      cscope = {
        db_file = "/home/wkatcher/cscope.out",
        exec = "cscope",
        picker = "quickfix",
        use_telescope =true,
        skip_picker_for_single_result = false,
        db_build_cmd_args = { "-bqv" },
      }
    },
  },]]

  {
    'github/copilot.vim',
  },

}

require("lazy").setup(plugins)

-- ================= Options ================= --

vim.o.termguicolors = true
vim.opt.showmode = false  -- don't need because of lualine
vim.cmd.colorscheme('tokyonight-night')

-- Indentation --
vim.o.tabstop = 4					-- maximum width of tab character (measured in spaces)
vim.bo.tabstop = 4
vim.o.shiftwidth = 4 	    -- size of indent (measured in spaces), should equal tabstop
vim.bo.shiftwidth = 4
vim.o.softtabstop = 4 	  -- should be the same as the other two above
vim.bo.softtabstop = 4
-- vim.o.expandtab = true    -- expand tabs to spaces
-- vim.bo.expandtab = true    -- expand tabs to spaces

-- [[ Search ]]
vim.o.ignorecase = true            -- bool: Ignore case in search patterns
vim.o.smartcase = true             -- bool: Override ignorecase if search contains capitals
vim.o.incsearch = true             -- bool: Use incremental search
--opt.hlsearch = false             -- bool: Highlight search matches

vim.wo.number = true
vim.wo.relativenumber = false
vim.o.autochdir = true
vim.cmd("autocmd BufEnter * set formatoptions-=ro")
vim.o.makeprg = 'make64'

-- lsp config
local lsp_zero = require('lsp-zero')

lsp_zero.on_attach(function(client, bufnr)
  -- see :help lsp-zero-keybindings
  -- to learn the available actions
  lsp_zero.default_keymaps({
    buffer = bufnr,
    preserve_mappings = false
  })
end)

require('lspconfig').clangd.setup{
  flags = {
    clangd = '/home/gbuilder/wkatcher/.vscode-server/data/User/globalStorage/llvm-vs-code-extensions.vscode-clangd/install/17.0.3/clangd_17.0.3/bin/clangd',
  }
}

-- ================= Key mappings ================= --

local map = vim.api.nvim_set_keymap
vim.g.mapleader = " "

-- ESC key is hard to get to
vim.keymap.set('i', 'jj', '<ESC>')

-- Keep cursor in center, eliminates cursor hunting
vim.keymap.set("n", "<C-d>", "<C-d>zz")
vim.keymap.set("n", "<C-u>", "<C-u>zz")
vim.keymap.set("n", "<C-f>", "<C-d>zz")
vim.keymap.set("n", "<C-b>", "<C-b>zz")
vim.keymap.set("n", "n", "nzzzv")
vim.keymap.set("n", "N", "Nzzzv")

-- quicker buffer switching
vim.keymap.set("n", '<leader>b', ":b#<CR>", {desc = "last used buffer"})
vim.keymap.set("n", '<leader>n', ":bn<CR>", {desc = "next buffer"})
vim.keymap.set("n", '<leader>p', ":bp<CR>", {desc = "previous buffer"})

local builtin = require('telescope.builtin')
vim.keymap.set('n', '<leader>ff', builtin.find_files, {desc = "telescope find_files"})
vim.keymap.set('n', '<leader>fg', builtin.live_grep, {desc = "telescope live_grep"})
vim.keymap.set('n', '<leader>fb', builtin.buffers, {desc = "telescope buffers"})
vim.keymap.set('n', '<leader>fh', builtin.help_tags, {desc = "telescope help_tags"})
vim.keymap.set('n', '<leader>fr', builtin.resume, {desc = "telescope resume"})
vim.keymap.set('n', '<leader>g', builtin.grep_string, {desc = "telescope grep_string"})

vim.keymap.set('n', '<leader>t', ':Neotree<CR>', {desc = "Neo-tree"})
vim.keymap.set('n', '<leader>z', ':ZenMode<CR>', {desc = "Zen mode"})
vim.keymap.set('n', '<leader>w', '<C-w><C-w>', {desc = "Next window"})

