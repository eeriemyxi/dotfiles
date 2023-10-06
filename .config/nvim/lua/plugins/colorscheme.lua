return {
	{
		"rebelot/kanagawa.nvim",
		lazy = false,
		-- priority = 1000,
		config = function()
			vim.cmd("colorscheme kanagawa")
		end,
	},
	{ "xiyaowong/transparent.nvim", enabled = true, priority = 900, lazy = false },
}
