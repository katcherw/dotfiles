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
    config = function()
      -- load the colorscheme here
      vim.cmd([[colorscheme tokyonight-night]])
    end,
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

  -- download and install LSP servers - enter :MasonInstall clangd the first time
  {
    'williamboman/mason.nvim',
    build = ":MasonUpdate",
    config = function()
      require("mason").setup()
    end
  },
  {
    'williamboman/mason-lspconfig.nvim',
    config = function()
      local servers = { 'rust_analyzer', 'clangd' }
      for _, lsp in pairs(servers) do
        require('lspconfig')[lsp].setup({
          on_attach = on_attach,
          flags = {
            -- This will be the default in neovim 0.7+
            debounce_text_changes = 150,
          }
        })
      end
      require("mason-lspconfig").setup()
    end, 
    dependencies = { {'williamboman/mason.nvim'} },
  },
  {
    "neovim/nvim-lspconfig",
    config = function()
       --require("nvim-lspconfig").clangd.setup({})
    end,
    dependencies = { {'williamboman/mason-lspconfig'} }
  },

  --[[
  {
    "p00f/clangd_extensions",
    config = function()   
      require("clangd_extensions").setup() { }
      end 
  },]]

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
  {'hrsh7th/cmp-nvim-lsp'},
  {'hrsh7th/cmp-nvim-lua'},
  {'hrsh7th/cmp-nvim-lsp-signature-help'},
  {'hrsh7th/cmp-vsnip'},
  {'hrsh7th/cmp-path'},                              
  {'hrsh7th/cmp-buffer'},                            
  {'hrsh7th/vim-vsnip'},     

  -- fuzzy finder
  {
    'nvim-telescope/telescope.nvim', tag = '0.1.1',
    dependencies = { {'nvim-lua/plenary.nvim'} }
  }, 

  -- neotree file explorer
  {
    "nvim-neo-tree/neo-tree.nvim",
    branch = "v2.x",
    dependencies = { 
      "nvim-lua/plenary.nvim",
      "nvim-tree/nvim-web-devicons", -- not strictly required, but recommended
      "MunifTanjim/nui.nvim",
    }
  }
}

require("lazy").setup(plugins)

-- ================= Options ================= --

-- Add lua/local.lua to add options for local server
local tmpf = io.open("lua/local.lua", "r")
if tmpf ~= nil then
  io.close(tmpf)  
  require("local")
end

vim.o.termguicolors = true
vim.opt.showmode = false  -- don't need because of lualine

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
vim.wo.relativenumber = true
vim.o.autochdir = true

--[[
local rt = require("rust-tools")
vim.keymap.set("n", "<C-d>", "<C-d>zz")
vim.keymap.set("n", "<C-u>", "<C-u>zz")
rt.setup({
  server = {
    on_attach = function(_, bufnr)
      -- Hover actions
      vim.keymap.set("n", "<C-space>", rt.hover_actions.hover_actions, { buffer = bufnr })
      -- Code action groups
      vim.keymap.set("n", "<Leader>a", rt.code_action_group.code_action_group, { buffer = bufnr })
    end,
  },
})


-- interact with rust-analyzer
local rt = require("rust-tools")
rt.setup({
  server = {
    on_attach = function(_, bufnr)
      -- Hover actions
      vim.keymap.set("n", "<C-space>", rt.hover_actions.hover_actions, { buffer = bufnr })
      -- Code action groups
      vim.keymap.set("n", "<Leader>a", rt.code_action_group.code_action_group, { buffer = bufnr })
    end,
  },
})

]]

-- Completion Plugin Setup
local cmp = require 'cmp'
cmp.setup({
  -- Enable LSP snippets
  snippet = {
    expand = function(args)
        vim.fn["vsnip#anonymous"](args.body)
    end,
  },
  mapping = {
    ['<C-p>'] = cmp.mapping.select_prev_item(),
    ['<C-n>'] = cmp.mapping.select_next_item(),
    -- Add tab support
    ['<S-Tab>'] = cmp.mapping.select_prev_item(),
    ['<Tab>'] = cmp.mapping.select_next_item(),
    ['<C-S-f>'] = cmp.mapping.scroll_docs(-4),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),
    ['<C-Space>'] = cmp.mapping.complete(),
    ['<C-e>'] = cmp.mapping.close(),
    ['<CR>'] = cmp.mapping.confirm({
      behavior = cmp.ConfirmBehavior.Insert,
      select = true,
    })
  },
  -- Installed sources:
  sources = {
    { name = 'path' },                              -- file paths
    { name = 'nvim_lsp', keyword_length = 3 },      -- from language server
    { name = 'nvim_lsp_signature_help'},            -- display function signatures with current parameter emphasized
    { name = 'nvim_lua', keyword_length = 2},       -- complete neovim's Lua runtime API such vim.lsp.*
    { name = 'buffer', keyword_length = 2 },        -- source current buffer
    { name = 'vsnip', keyword_length = 2 },         -- nvim-cmp source for vim-vsnip 
    { name = 'calc'},                               -- source for math calculation
  },
  window = {
      completion = cmp.config.window.bordered(),
      documentation = cmp.config.window.bordered(),
  },
  formatting = {
      fields = {'menu', 'abbr', 'kind'},
      format = function(entry, item)
          local menu_icon ={
              nvim_lsp = 'λ',
              vsnip = '⋗',
              buffer = 'Ω',
              path = '🖫',
          }
          item.menu = menu_icon[entry.source.name]
          return item
      end,
  },
})


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

local builtin = require('telescope.builtin')
vim.keymap.set('n', '<leader>ff', builtin.find_files, {desc = "telescope find_files"})
vim.keymap.set('n', '<leader>fg', builtin.live_grep, {desc = "telescope live_grep"})
vim.keymap.set('n', '<leader>fb', builtin.buffers, {desc = "telescope buffers"})
vim.keymap.set('n', '<leader>fh', builtin.help_tags, {desc = "telescope help_tags"})
vim.keymap.set('n', '<leader>g', builtin.grep_string, {desc = "telescope grep_string"})

vim.keymap.set('n', '<leader>t', ':Neotree<CR>', {desc = "Neo-tree"})

