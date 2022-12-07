-- Services:
local HttpService = game:GetService("HttpService")

-- Modules:
local loadstring = {}
do -- Loadstring functionality
	local LuaX, LuaZ, LuaK, LuaP, LuaY, LuaU, FiOne

	do -- LuaZ Module:
		local a = {}
		function a:make_getS(b)
			local c = b
			return function()
				if not c then
					return nil
				end
				local d = c
				c = nil
				return d
			end
		end
		function a:init(e, d, f)
			if not e then
				return
			end
			local g = {}
			g.reader = e
			g.data = d or ""
			g.name = f
			if not d or d == "" then
				g.n = 0
			else
				g.n = #d
			end
			g.p = 0
			return g
		end
		function a:fill(g)
			local b = g.reader()
			g.data = b
			if not b or b == "" then
				return "EOZ"
			end
			g.n, g.p = #b - 1, 1
			return string.sub(b, 1, 1)
		end
		function a:zgetc(g)
			local h, i = g.n, g.p + 1
			if h > 0 then
				g.n, g.p = h - 1, i
				return string.sub(g.data, i, i)
			else
				return self:fill(g)
			end
		end
		LuaZ = a
	end

	do -- LuaX Module:
		local a = require
		local b = LuaZ
		local c = {}
		c.RESERVED = [[
TK_AND and
TK_BREAK break
TK_DO do
TK_ELSE else
TK_ELSEIF elseif
TK_END end
TK_FALSE false
TK_FOR for
TK_FUNCTION function
TK_IF if
TK_IN in
TK_LOCAL local
TK_NIL nil
TK_NOT not
TK_OR or
TK_REPEAT repeat
TK_RETURN return
TK_THEN then
TK_TRUE true
TK_UNTIL until
TK_WHILE while
TK_CONCAT ..
TK_DOTS ...
TK_EQ ==
TK_GE >=
TK_LE <=
TK_NE ~=
TK_NAME <name>
TK_NUMBER <number>
TK_STRING <string>
TK_EOS <eof>]]
		c.MAXSRC = 80
		c.MAX_INT = 2147483645
		c.LUA_QS = "'%s'"
		c.LUA_COMPAT_LSTR = 1
		function c:init()
			local d, e = {}, {}
			for f in string.gmatch(self.RESERVED, "[^\n]+") do
				local g, g, h, i = string.find(f, "(%S+)%s+(%S+)")
				d[h] = i
				e[i] = h
			end
			self.tokens = d
			self.enums = e
		end
		function c:chunkid(j, k)
			local l
			local m = string.sub(j, 1, 1)
			if m == "=" then
				l = string.sub(j, 2, k)
			else
				if m == "@" then
					j = string.sub(j, 2)
					k = k - #" '...' "
					local n = #j
					l = ""
					if n > k then
						j = string.sub(j, 1 + n - k)
						l = l .. "..."
					end
					l = l .. j
				else
					local o = string.find(j, "[\n\r]")
					o = o and o - 1 or #j
					k = k - #' [string "..."] '
					if o > k then
						o = k
					end
					l = '[string "'
					if o < #j then
						l = l .. string.sub(j, 1, o) .. "..."
					else
						l = l .. j
					end
					l = l .. '"]'
				end
			end
			return l
		end
		function c:token2str(p, q)
			if string.sub(q, 1, 3) ~= "TK_" then
				if string.find(q, "%c") then
					return string.format("char(%d)", string.byte(q))
				end
				return q
			else
			end
			return self.tokens[q]
		end
		function c:lexerror(p, r, q)
			local function s(p, q)
				if q == "TK_NAME" or q == "TK_STRING" or q == "TK_NUMBER" then
					return p.buff
				else
					return self:token2str(p, q)
				end
			end
			local t = self:chunkid(p.source, self.MAXSRC)
			local r = string.format("%s:%d: %s", t, p.linenumber, r)
			if q then
				r = string.format("%s near " .. self.LUA_QS, r, s(p, q))
			end
			error(r)
		end
		function c:syntaxerror(p, r)
			self:lexerror(p, r, p.t.token)
		end
		function c:currIsNewline(p)
			return p.current == "\n" or p.current == "\r"
		end

		function c:inclinenumber(p)
			local u = p.current
			self:nextc(p)
			if self:currIsNewline(p) and p.current ~= u then
				self:nextc(p)
			end
			p.linenumber = p.linenumber + 1
			if p.linenumber >= self.MAX_INT then
				self:syntaxerror(p, "chunk has too many lines")
			end
		end
		function c:setinput(v, p, w, j)
			if not p then
				p = {}
			end
			if not p.lookahead then
				p.lookahead = {}
			end
			if not p.t then
				p.t = {}
			end
			p.decpoint = "."
			p.L = v
			p.lookahead.token = "TK_EOS"
			p.z = w
			p.fs = nil
			p.linenumber = 1
			p.lastline = 1
			p.source = j
			self:nextc(p)
		end
		function c:check_next(p, x)
			if not string.find(x, p.current, 1, 1) then
				return false
			end
			self:save_and_next(p)
			return true
		end
		function c:next(p)
			p.lastline = p.linenumber
			if p.lookahead.token ~= "TK_EOS" then
				p.t.seminfo = p.lookahead.seminfo
				p.t.token = p.lookahead.token
				p.lookahead.token = "TK_EOS"
			else
				p.t.token = self:llex(p, p.t)
			end
		end
		function c:lookahead(p)
			p.lookahead.token = self:llex(p, p.lookahead)
		end
		function c:nextc(p)
			local y = b:zgetc(p.z)
			p.current = y
			return y
		end
		function c:save(p, y)
			local t = p.buff
			p.buff = t .. y
		end
		function c:save_and_next(p)
			self:save(p, p.current)
			return self:nextc(p)
		end
		function c:str2d(z)
			local A = tonumber(z)
			if A then
				return A
			end
			if string.lower(string.sub(z, 1, 2)) == "0x" then
				A = tonumber(z, 16)
				if A then
					return A
				end
			end
			return nil
		end
		function c:buffreplace(p, B, C)
			local A, t = "", p.buff
			for D = 1, #t do
				local y = string.sub(t, D, D)
				if y == B then
					y = C
				end
				A = A .. y
			end
			p.buff = A
		end
		function c:trydecpoint(p, E)
			local u = p.decpoint
			self:buffreplace(p, u, p.decpoint)
			local F = self:str2d(p.buff)
			E.seminfo = F
			if not F then
				self:buffreplace(p, p.decpoint, ".")
				self:lexerror(p, "malformed number", "TK_NUMBER")
			end
		end
		function c:read_numeral(p, E)
			repeat
				self:save_and_next(p)
			until string.find(p.current, "%D") and p.current ~= "."
			if self:check_next(p, "Ee") then
				self:check_next(p, "+-")
			end
			while string.find(p.current, "^%w$") or p.current == "_" do
				self:save_and_next(p)
			end
			self:buffreplace(p, ".", p.decpoint)
			local F = self:str2d(p.buff)
			E.seminfo = F
			if not F then
				self:trydecpoint(p, E)
			end
		end
		function c:skip_sep(p)
			local G = 0
			local z = p.current
			self:save_and_next(p)
			while p.current == "=" do
				self:save_and_next(p)
				G = G + 1
			end
			return p.current == z and G or -G - 1
		end
		function c:read_long_string(p, E, H)
			local I = 0
			self:save_and_next(p)
			if self:currIsNewline(p) then
				self:inclinenumber(p)
			end
			while true do
				local y = p.current
				if y == "EOZ" then
					self:lexerror(p, E and "unfinished long string" or "unfinished long comment", "TK_EOS")
				elseif y == "[" then
					if self.LUA_COMPAT_LSTR then
						if self:skip_sep(p) == H then
							self:save_and_next(p)
							I = I + 1
							if self.LUA_COMPAT_LSTR == 1 then
								if H == 0 then
									self:lexerror(p, "nesting of [[...]] is deprecated", "[")
								end
							end
						end
					end
				elseif y == "]" then
					if self:skip_sep(p) == H then
						self:save_and_next(p)
						if self.LUA_COMPAT_LSTR and self.LUA_COMPAT_LSTR == 2 then
							I = I - 1
							if H == 0 and I >= 0 then
								break
							end
						end
						break
					end
				elseif self:currIsNewline(p) then
					self:save(p, "\n")
					self:inclinenumber(p)
					if not E then
						p.buff = ""
					end
				else
					if E then
						self:save_and_next(p)
					else
						self:nextc(p)
					end
				end
			end
			if E then
				local D = 3 + H
				E.seminfo = string.sub(p.buff, D, -D)
			end
		end
		function c:read_string(p, J, E)
			self:save_and_next(p)
			while p.current ~= J do
				local y = p.current
				if y == "EOZ" then
					self:lexerror(p, "unfinished string", "TK_EOS")
				elseif self:currIsNewline(p) then
					self:lexerror(p, "unfinished string", "TK_STRING")
				elseif y == "\\" then
					y = self:nextc(p)
					if self:currIsNewline(p) then
						self:save(p, "\n")
						self:inclinenumber(p)
					elseif y ~= "EOZ" then
						local K = string.find("abfnrtv", y, 1, 1)
						if K then
							self:save(p, string.sub("\a\b\f\n\r\t\v", K, K))
							self:nextc(p)
						elseif not string.find(y, "%d") then
							self:save_and_next(p)
						else
							y, K = 0, 0
							repeat
								y = 10 * y + p.current
								self:nextc(p)
								K = K + 1
							until K >= 3 or not string.find(p.current, "%d")
							if y > 255 then
								self:lexerror(p, "escape sequence too large", "TK_STRING")
							end
							self:save(p, string.char(y))
						end
					end
				else
					self:save_and_next(p)
				end
			end
			self:save_and_next(p)
			E.seminfo = string.sub(p.buff, 2, -2)
		end
		function c:llex(p, E)
			p.buff = ""
			while true do
				local y = p.current
				if self:currIsNewline(p) then
					self:inclinenumber(p)
				elseif y == "-" then
					y = self:nextc(p)
					if y ~= "-" then
						return "-"
					end
					local H = -1
					if self:nextc(p) == "[" then
						H = self:skip_sep(p)
						p.buff = ""
					end
					if H >= 0 then
						self:read_long_string(p, nil, H)
						p.buff = ""
					else
						while not self:currIsNewline(p) and p.current ~= "EOZ" do
							self:nextc(p)
						end
					end
				elseif y == "[" then
					local H = self:skip_sep(p)
					if H >= 0 then
						self:read_long_string(p, E, H)
						return "TK_STRING"
					elseif H == -1 then
						return "["
					else
						self:lexerror(p, "invalid long string delimiter", "TK_STRING")
					end
				elseif y == "=" then
					y = self:nextc(p)
					if y ~= "=" then
						return "="
					else
						self:nextc(p)
						return "TK_EQ"
					end
				elseif y == "<" then
					y = self:nextc(p)
					if y ~= "=" then
						return "<"
					else
						self:nextc(p)
						return "TK_LE"
					end
				elseif y == ">" then
					y = self:nextc(p)
					if y ~= "=" then
						return ">"
					else
						self:nextc(p)
						return "TK_GE"
					end
				elseif y == "~" then
					y = self:nextc(p)
					if y ~= "=" then
						return "~"
					else
						self:nextc(p)
						return "TK_NE"
					end
				elseif y == '"' or y == "'" then
					self:read_string(p, y, E)
					return "TK_STRING"
				elseif y == "." then
					y = self:save_and_next(p)
					if self:check_next(p, ".") then
						if self:check_next(p, ".") then
							return "TK_DOTS"
						else
							return "TK_CONCAT"
						end
					elseif not string.find(y, "%d") then
						return "."
					else
						self:read_numeral(p, E)
						return "TK_NUMBER"
					end
				elseif y == "EOZ" then
					return "TK_EOS"
				else
					if string.find(y, "%s") then
						self:nextc(p)
					elseif string.find(y, "%d") then
						self:read_numeral(p, E)
						return "TK_NUMBER"
					elseif string.find(y, "[_%a]") then
						repeat
							y = self:save_and_next(p)
						until y == "EOZ" or not string.find(y, "[_%w]")
						local L = p.buff
						local h = self.enums[L]
						if h then
							return h
						end
						E.seminfo = L
						return "TK_NAME"
					else
						self:nextc(p)
						return y
					end
				end
			end
		end

		LuaX = c
	end

	do -- LuaP Module:
		local a = {}
		a.OpMode = { iABC = 0, iABx = 1, iAsBx = 2 }
		a.SIZE_C = 9
		a.SIZE_B = 9
		a.SIZE_Bx = a.SIZE_C + a.SIZE_B
		a.SIZE_A = 8
		a.SIZE_OP = 6
		a.POS_OP = 0
		a.POS_A = a.POS_OP + a.SIZE_OP
		a.POS_C = a.POS_A + a.SIZE_A
		a.POS_B = a.POS_C + a.SIZE_C
		a.POS_Bx = a.POS_C
		a.MAXARG_Bx = math.ldexp(1, a.SIZE_Bx) - 1
		a.MAXARG_sBx = math.floor(a.MAXARG_Bx / 2)
		a.MAXARG_A = math.ldexp(1, a.SIZE_A) - 1
		a.MAXARG_B = math.ldexp(1, a.SIZE_B) - 1
		a.MAXARG_C = math.ldexp(1, a.SIZE_C) - 1
		function a:GET_OPCODE(b)
			return self.ROpCode[b.OP]
		end
		function a:SET_OPCODE(b, c)
			b.OP = self.OpCode[c]
		end
		function a:GETARG_A(b)
			return b.A
		end
		function a:SETARG_A(b, d)
			b.A = d
		end
		function a:GETARG_B(b)
			return b.B
		end
		function a:SETARG_B(b, e)
			b.B = e
		end
		function a:GETARG_C(b)
			return b.C
		end
		function a:SETARG_C(b, e)
			b.C = e
		end
		function a:GETARG_Bx(b)
			return b.Bx
		end
		function a:SETARG_Bx(b, e)
			b.Bx = e
		end
		function a:GETARG_sBx(b)
			return b.Bx - self.MAXARG_sBx
		end
		function a:SETARG_sBx(b, e)
			b.Bx = e + self.MAXARG_sBx
		end
		function a:CREATE_ABC(c, f, e, g)
			return { OP = self.OpCode[c], A = f, B = e, C = g }
		end
		function a:CREATE_ABx(c, f, h)
			return { OP = self.OpCode[c], A = f, Bx = h }
		end
		function a:CREATE_Inst(g)
			local c = g % 64
			g = (g - c) / 64
			local f = g % 256
			g = (g - f) / 256
			return self:CREATE_ABx(c, f, g)
		end
		function a:Instruction(b)
			if b.Bx then
				b.C = b.Bx % 512
				b.B = (b.Bx - b.C) / 512
			end
			local i = b.A * 64 + b.OP
			local j = i % 256
			i = b.C * 64 + (i - j) / 256
			local k = i % 256
			i = b.B * 128 + (i - k) / 256
			local l = i % 256
			local m = (i - l) / 256
			return string.char(j, k, l, m)
		end
		function a:DecodeInst(n)
			local o = string.byte
			local b = {}
			local i = o(n, 1)
			local p = i % 64
			b.OP = p
			i = o(n, 2) * 4 + (i - p) / 64
			local f = i % 256
			b.A = f
			i = o(n, 3) * 4 + (i - f) / 256
			local g = i % 512
			b.C = g
			b.B = o(n, 4) * 2 + (i - g) / 512
			local q = self.OpMode[tonumber(string.sub(self.opmodes[p + 1], 7, 7))]
			if q ~= "iABC" then
				b.Bx = b.B * 512 + b.C
			end
			return b
		end
		a.BITRK = math.ldexp(1, a.SIZE_B - 1)
		function a:ISK(n)
			return n >= self.BITRK
		end
		function a:INDEXK(n)
			return n - self.BITRK
		end
		a.MAXINDEXRK = a.BITRK - 1
		function a:RKASK(n)
			return n + self.BITRK
		end
		a.NO_REG = a.MAXARG_A
		a.opnames = {}
		a.OpCode = {}
		a.ROpCode = {}
		local b = 0
		for r in
			string.gmatch(
				[[
MOVE LOADK LOADBOOL LOADNIL GETUPVAL
GETGLOBAL GETTABLE SETGLOBAL SETUPVAL SETTABLE
NEWTABLE SELF ADD SUB MUL
DIV MOD POW UNM NOT
LEN CONCAT JMP EQ LT
LE TEST TESTSET CALL TAILCALL
RETURN FORLOOP FORPREP TFORLOOP SETLIST
CLOSE CLOSURE VARARG
]],
				"%S+"
			)
		do
			local s = "OP_" .. r
			a.opnames[b] = r
			a.OpCode[s] = b
			a.ROpCode[b] = s
			b = b + 1
		end
		a.NUM_OPCODES = b
		a.OpArgMask = { OpArgN = 0, OpArgU = 1, OpArgR = 2, OpArgK = 3 }
		function a:getOpMode(t)
			return self.opmodes[self.OpCode[t]] % 4
		end
		function a:getBMode(t)
			return math.floor(self.opmodes[self.OpCode[t]] / 16) % 4
		end
		function a:getCMode(t)
			return math.floor(self.opmodes[self.OpCode[t]] / 4) % 4
		end
		function a:testAMode(t)
			return math.floor(self.opmodes[self.OpCode[t]] / 64) % 2
		end
		function a:testTMode(t)
			return math.floor(self.opmodes[self.OpCode[t]] / 128)
		end
		a.LFIELDS_PER_FLUSH = 50
		local function q(u, f, e, g, t)
			local a = a
			return u * 128 + f * 64 + a.OpArgMask[e] * 16 + a.OpArgMask[g] * 4 + a.OpMode[t]
		end
		a.opmodes = {
			q(0, 1, "OpArgK", "OpArgN", "iABx"),
			q(0, 1, "OpArgU", "OpArgU", "iABC"),
			q(0, 1, "OpArgR", "OpArgN", "iABC"),
			q(0, 1, "OpArgU", "OpArgN", "iABC"),
			q(0, 1, "OpArgK", "OpArgN", "iABx"),
			q(0, 1, "OpArgR", "OpArgK", "iABC"),
			q(0, 0, "OpArgK", "OpArgN", "iABx"),
			q(0, 0, "OpArgU", "OpArgN", "iABC"),
			q(0, 0, "OpArgK", "OpArgK", "iABC"),
			q(0, 1, "OpArgU", "OpArgU", "iABC"),
			q(0, 1, "OpArgR", "OpArgK", "iABC"),
			q(0, 1, "OpArgK", "OpArgK", "iABC"),
			q(0, 1, "OpArgK", "OpArgK", "iABC"),
			q(0, 1, "OpArgK", "OpArgK", "iABC"),
			q(0, 1, "OpArgK", "OpArgK", "iABC"),
			q(0, 1, "OpArgK", "OpArgK", "iABC"),
			q(0, 1, "OpArgK", "OpArgK", "iABC"),
			q(0, 1, "OpArgR", "OpArgN", "iABC"),
			q(0, 1, "OpArgR", "OpArgN", "iABC"),
			q(0, 1, "OpArgR", "OpArgN", "iABC"),
			q(0, 1, "OpArgR", "OpArgR", "iABC"),
			q(0, 0, "OpArgR", "OpArgN", "iAsBx"),
			q(1, 0, "OpArgK", "OpArgK", "iABC"),
			q(1, 0, "OpArgK", "OpArgK", "iABC"),
			q(1, 0, "OpArgK", "OpArgK", "iABC"),
			q(1, 1, "OpArgR", "OpArgU", "iABC"),
			q(1, 1, "OpArgR", "OpArgU", "iABC"),
			q(0, 1, "OpArgU", "OpArgU", "iABC"),
			q(0, 1, "OpArgU", "OpArgU", "iABC"),
			q(0, 0, "OpArgU", "OpArgN", "iABC"),
			q(0, 1, "OpArgR", "OpArgN", "iAsBx"),
			q(0, 1, "OpArgR", "OpArgN", "iAsBx"),
			q(1, 0, "OpArgN", "OpArgU", "iABC"),
			q(0, 0, "OpArgU", "OpArgU", "iABC"),
			q(0, 0, "OpArgN", "OpArgN", "iABC"),
			q(0, 1, "OpArgU", "OpArgN", "iABx"),
			q(0, 1, "OpArgU", "OpArgN", "iABC"),
		}
		a.opmodes[0] = q(0, 1, "OpArgR", "OpArgN", "iABC")
		LuaP = a
	end

	do -- LuaK Module:
		local a = {}
		local c = LuaP
		local d = LuaX
		a.MAXSTACK = 250
		function a:ttisnumber(e)
			if e then
				return type(e.value) == "number"
			else
				return false
			end
		end
		function a:nvalue(e)
			return e.value
		end
		function a:setnilvalue(e)
			e.value = nil
		end
		function a:setsvalue(e, f)
			e.value = f
		end
		a.setnvalue = a.setsvalue
		a.sethvalue = a.setsvalue
		a.setbvalue = a.setsvalue
		function a:numadd(g, h)
			return g + h
		end
		function a:numsub(g, h)
			return g - h
		end
		function a:nummul(g, h)
			return g * h
		end
		function a:numdiv(g, h)
			return g / h
		end
		function a:nummod(g, h)
			return g % h
		end
		function a:numpow(g, h)
			return g ^ h
		end
		function a:numunm(g)
			return -g
		end
		function a:numisnan(g)
			return not g == g
		end
		a.NO_JUMP = -1
		a.BinOpr = {
			OPR_ADD = 0,
			OPR_SUB = 1,
			OPR_MUL = 2,
			OPR_DIV = 3,
			OPR_MOD = 4,
			OPR_POW = 5,
			OPR_CONCAT = 6,
			OPR_NE = 7,
			OPR_EQ = 8,
			OPR_LT = 9,
			OPR_LE = 10,
			OPR_GT = 11,
			OPR_GE = 12,
			OPR_AND = 13,
			OPR_OR = 14,
			OPR_NOBINOPR = 15,
		}
		a.UnOpr = { OPR_MINUS = 0, OPR_NOT = 1, OPR_LEN = 2, OPR_NOUNOPR = 3 }
		function a:getcode(i, j)
			return i.f.code[j.info]
		end
		function a:codeAsBx(i, e, k, l)
			return self:codeABx(i, e, k, l + c.MAXARG_sBx)
		end
		function a:setmultret(i, j)
			self:setreturns(i, j, LuaY.LUA_MULTRET)
		end
		function a:hasjumps(j)
			return j.t ~= j.f
		end
		function a:isnumeral(j)
			return j.k == "VKNUM" and j.t == self.NO_JUMP and j.f == self.NO_JUMP
		end
		function a:_nil(i, m, n)
			if i.pc > i.lasttarget then
				if i.pc == 0 then
					if m >= i.nactvar then
						return
					end
				else
					local o = i.f.code[i.pc - 1]
					if c:GET_OPCODE(o) == "OP_LOADNIL" then
						local p = c:GETARG_A(o)
						local q = c:GETARG_B(o)
						if p <= m and m <= q + 1 then
							if m + n - 1 > q then
								c:SETARG_B(o, m + n - 1)
							end
							return
						end
					end
				end
			end
			self:codeABC(i, "OP_LOADNIL", m, m + n - 1, 0)
		end
		function a:jump(i)
			local r = i.jpc
			i.jpc = self.NO_JUMP
			local s = self:codeAsBx(i, "OP_JMP", 0, self.NO_JUMP)
			s = self:concat(i, s, r)
			return s
		end
		function a:ret(i, t, u)
			self:codeABC(i, "OP_RETURN", t, u + 1, 0)
		end
		function a:condjump(i, v, k, w, x)
			self:codeABC(i, v, k, w, x)
			return self:jump(i)
		end
		function a:fixjump(i, y, z)
			local A = i.f.code[y]
			local B = z - (y + 1)
			assert(z ~= self.NO_JUMP)
			if math.abs(B) > c.MAXARG_sBx then
				d:syntaxerror(i.ls, "control structure too long")
			end
			c:SETARG_sBx(A, B)
		end
		function a:getlabel(i)
			i.lasttarget = i.pc
			return i.pc
		end
		function a:getjump(i, y)
			local B = c:GETARG_sBx(i.f.code[y])
			if B == self.NO_JUMP then
				return self.NO_JUMP
			else
				return y + 1 + B
			end
		end
		function a:getjumpcontrol(i, y)
			local C = i.f.code[y]
			local D = i.f.code[y - 1]
			if y >= 1 and c:testTMode(c:GET_OPCODE(D)) ~= 0 then
				return D
			else
				return C
			end
		end
		function a:need_value(i, E)
			while E ~= self.NO_JUMP do
				local F = self:getjumpcontrol(i, E)
				if c:GET_OPCODE(F) ~= "OP_TESTSET" then
					return true
				end
				E = self:getjump(i, E)
			end
			return false
		end
		function a:patchtestreg(i, G, H)
			local F = self:getjumpcontrol(i, G)
			if c:GET_OPCODE(F) ~= "OP_TESTSET" then
				return false
			end
			if H ~= c.NO_REG and H ~= c:GETARG_B(F) then
				c:SETARG_A(F, H)
			else
				c:SET_OPCODE(F, "OP_TEST")
				local h = c:GETARG_B(F)
				c:SETARG_A(F, h)
				c:SETARG_B(F, 0)
			end
			return true
		end
		function a:removevalues(i, E)
			while E ~= self.NO_JUMP do
				self:patchtestreg(i, E, c.NO_REG)
				E = self:getjump(i, E)
			end
		end
		function a:patchlistaux(i, E, I, H, J)
			while E ~= self.NO_JUMP do
				local K = self:getjump(i, E)
				if self:patchtestreg(i, E, H) then
					self:fixjump(i, E, I)
				else
					self:fixjump(i, E, J)
				end
				E = K
			end
		end
		function a:dischargejpc(i)
			self:patchlistaux(i, i.jpc, i.pc, c.NO_REG, i.pc)
			i.jpc = self.NO_JUMP
		end
		function a:patchlist(i, E, L)
			if L == i.pc then
				self:patchtohere(i, E)
			else
				assert(L < i.pc)
				self:patchlistaux(i, E, L, c.NO_REG, L)
			end
		end
		function a:patchtohere(i, E)
			self:getlabel(i)
			i.jpc = self:concat(i, i.jpc, E)
		end
		function a:concat(i, M, N)
			if N == self.NO_JUMP then
				return M
			elseif M == self.NO_JUMP then
				return N
			else
				local E = M
				local K = self:getjump(i, E)
				while K ~= self.NO_JUMP do
					E = K
					K = self:getjump(i, E)
				end
				self:fixjump(i, E, N)
			end
			return M
		end
		function a:checkstack(i, n)
			local O = i.freereg + n
			if O > i.f.maxstacksize then
				if O >= self.MAXSTACK then
					d:syntaxerror(i.ls, "function or expression too complex")
				end
				i.f.maxstacksize = O
			end
		end
		function a:reserveregs(i, n)
			self:checkstack(i, n)
			i.freereg = i.freereg + n
		end
		function a:freereg(i, H)
			if not c:ISK(H) and H >= i.nactvar then
				i.freereg = i.freereg - 1
				assert(H == i.freereg)
			end
		end
		function a:freeexp(i, j)
			if j.k == "VNONRELOC" then
				self:freereg(i, j.info)
			end
		end
		function a:addk(i, P, Q)
			local R = i.L
			local S = i.h[P.value]
			local T = i.f
			if self:ttisnumber(S) then
				return self:nvalue(S)
			else
				S = {}
				self:setnvalue(S, i.nk)
				i.h[P.value] = S
				LuaY:growvector(R, T.k, i.nk, T.sizek, nil, c.MAXARG_Bx, "constant table overflow")
				T.k[i.nk] = Q
				local U = i.nk
				i.nk = i.nk + 1
				return U
			end
		end
		function a:stringK(i, V)
			local e = {}
			self:setsvalue(e, V)
			return self:addk(i, e, e)
		end
		function a:numberK(i, W)
			local e = {}
			self:setnvalue(e, W)
			return self:addk(i, e, e)
		end
		function a:boolK(i, h)
			local e = {}
			self:setbvalue(e, h)
			return self:addk(i, e, e)
		end
		function a:nilK(i)
			local P, Q = {}, {}
			self:setnilvalue(Q)
			self:sethvalue(P, i.h)
			return self:addk(i, P, Q)
		end
		function a:setreturns(i, j, X)
			if j.k == "VCALL" then
				c:SETARG_C(self:getcode(i, j), X + 1)
			elseif j.k == "VVARARG" then
				c:SETARG_B(self:getcode(i, j), X + 1)
				c:SETARG_A(self:getcode(i, j), i.freereg)
				a:reserveregs(i, 1)
			end
		end
		function a:setoneret(i, j)
			if j.k == "VCALL" then
				j.k = "VNONRELOC"
				j.info = c:GETARG_A(self:getcode(i, j))
			elseif j.k == "VVARARG" then
				c:SETARG_B(self:getcode(i, j), 2)
				j.k = "VRELOCABLE"
			end
		end
		function a:dischargevars(i, j)
			local P = j.k
			if P == "VLOCAL" then
				j.k = "VNONRELOC"
			elseif P == "VUPVAL" then
				j.info = self:codeABC(i, "OP_GETUPVAL", 0, j.info, 0)
				j.k = "VRELOCABLE"
			elseif P == "VGLOBAL" then
				j.info = self:codeABx(i, "OP_GETGLOBAL", 0, j.info)
				j.k = "VRELOCABLE"
			elseif P == "VINDEXED" then
				self:freereg(i, j.aux)
				self:freereg(i, j.info)
				j.info = self:codeABC(i, "OP_GETTABLE", 0, j.info, j.aux)
				j.k = "VRELOCABLE"
			elseif P == "VVARARG" or P == "VCALL" then
				self:setoneret(i, j)
			else
			end
		end
		function a:code_label(i, k, h, Y)
			self:getlabel(i)
			return self:codeABC(i, "OP_LOADBOOL", k, h, Y)
		end
		function a:discharge2reg(i, j, H)
			self:dischargevars(i, j)
			local P = j.k
			if P == "VNIL" then
				self:_nil(i, H, 1)
			elseif P == "VFALSE" or P == "VTRUE" then
				self:codeABC(i, "OP_LOADBOOL", H, j.k == "VTRUE" and 1 or 0, 0)
			elseif P == "VK" then
				self:codeABx(i, "OP_LOADK", H, j.info)
			elseif P == "VKNUM" then
				self:codeABx(i, "OP_LOADK", H, self:numberK(i, j.nval))
			elseif P == "VRELOCABLE" then
				local y = self:getcode(i, j)
				c:SETARG_A(y, H)
			elseif P == "VNONRELOC" then
				if H ~= j.info then
					self:codeABC(i, "OP_MOVE", H, j.info, 0)
				end
			else
				assert(j.k == "VVOID" or j.k == "VJMP")
				return
			end
			j.info = H
			j.k = "VNONRELOC"
		end
		function a:discharge2anyreg(i, j)
			if j.k ~= "VNONRELOC" then
				self:reserveregs(i, 1)
				self:discharge2reg(i, j, i.freereg - 1)
			end
		end
		function a:exp2reg(i, j, H)
			self:discharge2reg(i, j, H)
			if j.k == "VJMP" then
				j.t = self:concat(i, j.t, j.info)
			end
			if self:hasjumps(j) then
				local Z
				local _ = self.NO_JUMP
				local a0 = self.NO_JUMP
				if self:need_value(i, j.t) or self:need_value(i, j.f) then
					local a1 = j.k == "VJMP" and self.NO_JUMP or self:jump(i)
					_ = self:code_label(i, H, 0, 1)
					a0 = self:code_label(i, H, 1, 0)
					self:patchtohere(i, a1)
				end
				Z = self:getlabel(i)
				self:patchlistaux(i, j.f, Z, H, _)
				self:patchlistaux(i, j.t, Z, H, a0)
			end
			j.f, j.t = self.NO_JUMP, self.NO_JUMP
			j.info = H
			j.k = "VNONRELOC"
		end
		function a:exp2nextreg(i, j)
			self:dischargevars(i, j)
			self:freeexp(i, j)
			self:reserveregs(i, 1)
			self:exp2reg(i, j, i.freereg - 1)
		end
		function a:exp2anyreg(i, j)
			self:dischargevars(i, j)
			if j.k == "VNONRELOC" then
				if not self:hasjumps(j) then
					return j.info
				end
				if j.info >= i.nactvar then
					self:exp2reg(i, j, j.info)
					return j.info
				end
			end
			self:exp2nextreg(i, j)
			return j.info
		end
		function a:exp2val(i, j)
			if self:hasjumps(j) then
				self:exp2anyreg(i, j)
			else
				self:dischargevars(i, j)
			end
		end
		function a:exp2RK(i, j)
			self:exp2val(i, j)
			local P = j.k
			if P == "VKNUM" or P == "VTRUE" or P == "VFALSE" or P == "VNIL" then
				if i.nk <= c.MAXINDEXRK then
					if j.k == "VNIL" then
						j.info = self:nilK(i)
					else
						j.info = j.k == "VKNUM" and self:numberK(i, j.nval) or self:boolK(i, j.k == "VTRUE")
					end
					j.k = "VK"
					return c:RKASK(j.info)
				end
			elseif P == "VK" then
				if j.info <= c.MAXINDEXRK then
					return c:RKASK(j.info)
				end
			else
			end
			return self:exp2anyreg(i, j)
		end
		function a:storevar(i, a2, a3)
			local P = a2.k
			if P == "VLOCAL" then
				self:freeexp(i, a3)
				self:exp2reg(i, a3, a2.info)
				return
			elseif P == "VUPVAL" then
				local j = self:exp2anyreg(i, a3)
				self:codeABC(i, "OP_SETUPVAL", j, a2.info, 0)
			elseif P == "VGLOBAL" then
				local j = self:exp2anyreg(i, a3)
				self:codeABx(i, "OP_SETGLOBAL", j, a2.info)
			elseif P == "VINDEXED" then
				local j = self:exp2RK(i, a3)
				self:codeABC(i, "OP_SETTABLE", a2.info, a2.aux, j)
			else
				assert(0)
			end
			self:freeexp(i, a3)
		end
		function a:_self(i, j, a4)
			self:exp2anyreg(i, j)
			self:freeexp(i, j)
			local a5 = i.freereg
			self:reserveregs(i, 2)
			self:codeABC(i, "OP_SELF", a5, j.info, self:exp2RK(i, a4))
			self:freeexp(i, a4)
			j.info = a5
			j.k = "VNONRELOC"
		end
		function a:invertjump(i, j)
			local y = self:getjumpcontrol(i, j.info)
			assert(
				c:testTMode(c:GET_OPCODE(y)) ~= 0 and c:GET_OPCODE(y) ~= "OP_TESTSET" and c:GET_OPCODE(y) ~= "OP_TEST"
			)
			c:SETARG_A(y, c:GETARG_A(y) == 0 and 1 or 0)
		end
		function a:jumponcond(i, j, a6)
			if j.k == "VRELOCABLE" then
				local a7 = self:getcode(i, j)
				if c:GET_OPCODE(a7) == "OP_NOT" then
					i.pc = i.pc - 1
					return self:condjump(i, "OP_TEST", c:GETARG_B(a7), 0, a6 and 0 or 1)
				end
			end
			self:discharge2anyreg(i, j)
			self:freeexp(i, j)
			return self:condjump(i, "OP_TESTSET", c.NO_REG, j.info, a6 and 1 or 0)
		end
		function a:goiftrue(i, j)
			local y
			self:dischargevars(i, j)
			local P = j.k
			if P == "VK" or P == "VKNUM" or P == "VTRUE" then
				y = self.NO_JUMP
			elseif P == "VFALSE" then
				y = self:jump(i)
			elseif P == "VJMP" then
				self:invertjump(i, j)
				y = j.info
			else
				y = self:jumponcond(i, j, false)
			end
			j.f = self:concat(i, j.f, y)
			self:patchtohere(i, j.t)
			j.t = self.NO_JUMP
		end
		function a:goiffalse(i, j)
			local y
			self:dischargevars(i, j)
			local P = j.k
			if P == "VNIL" or P == "VFALSE" then
				y = self.NO_JUMP
			elseif P == "VTRUE" then
				y = self:jump(i)
			elseif P == "VJMP" then
				y = j.info
			else
				y = self:jumponcond(i, j, true)
			end
			j.t = self:concat(i, j.t, y)
			self:patchtohere(i, j.f)
			j.f = self.NO_JUMP
		end
		function a:codenot(i, j)
			self:dischargevars(i, j)
			local P = j.k
			if P == "VNIL" or P == "VFALSE" then
				j.k = "VTRUE"
			elseif P == "VK" or P == "VKNUM" or P == "VTRUE" then
				j.k = "VFALSE"
			elseif P == "VJMP" then
				self:invertjump(i, j)
			elseif P == "VRELOCABLE" or P == "VNONRELOC" then
				self:discharge2anyreg(i, j)
				self:freeexp(i, j)
				j.info = self:codeABC(i, "OP_NOT", 0, j.info, 0)
				j.k = "VRELOCABLE"
			else
				assert(0)
			end
			j.f, j.t = j.t, j.f
			self:removevalues(i, j.f)
			self:removevalues(i, j.t)
		end
		function a:indexed(i, a8, P)
			a8.aux = self:exp2RK(i, P)
			a8.k = "VINDEXED"
		end
		function a:constfolding(v, a9, aa)
			local W
			if not self:isnumeral(a9) or not self:isnumeral(aa) then
				return false
			end
			local ab = a9.nval
			local ac = aa.nval
			if v == "OP_ADD" then
				W = self:numadd(ab, ac)
			elseif v == "OP_SUB" then
				W = self:numsub(ab, ac)
			elseif v == "OP_MUL" then
				W = self:nummul(ab, ac)
			elseif v == "OP_DIV" then
				if ac == 0 then
					return false
				end
				W = self:numdiv(ab, ac)
			elseif v == "OP_MOD" then
				if ac == 0 then
					return false
				end
				W = self:nummod(ab, ac)
			elseif v == "OP_POW" then
				W = self:numpow(ab, ac)
			elseif v == "OP_UNM" then
				W = self:numunm(ab)
			elseif v == "OP_LEN" then
				return false
			else
				assert(0)
				W = 0
			end
			if self:numisnan(W) then
				return false
			end
			a9.nval = W
			return true
		end
		function a:codearith(i, v, a9, aa)
			if self:constfolding(v, a9, aa) then
				return
			else
				local ad = v ~= "OP_UNM" and v ~= "OP_LEN" and self:exp2RK(i, aa) or 0
				local ae = self:exp2RK(i, a9)
				if ae > ad then
					self:freeexp(i, a9)
					self:freeexp(i, aa)
				else
					self:freeexp(i, aa)
					self:freeexp(i, a9)
				end
				a9.info = self:codeABC(i, v, 0, ae, ad)
				a9.k = "VRELOCABLE"
			end
		end
		function a:codecomp(i, v, a6, a9, aa)
			local ae = self:exp2RK(i, a9)
			local ad = self:exp2RK(i, aa)
			self:freeexp(i, aa)
			self:freeexp(i, a9)
			if a6 == 0 and v ~= "OP_EQ" then
				ae, ad = ad, ae
				a6 = 1
			end
			a9.info = self:condjump(i, v, a6, ae, ad)
			a9.k = "VJMP"
		end
		function a:prefix(i, v, j)
			local aa = {}
			aa.t, aa.f = self.NO_JUMP, self.NO_JUMP
			aa.k = "VKNUM"
			aa.nval = 0
			if v == "OPR_MINUS" then
				if not self:isnumeral(j) then
					self:exp2anyreg(i, j)
				end
				self:codearith(i, "OP_UNM", j, aa)
			elseif v == "OPR_NOT" then
				self:codenot(i, j)
			elseif v == "OPR_LEN" then
				self:exp2anyreg(i, j)
				self:codearith(i, "OP_LEN", j, aa)
			else
				assert(0)
			end
		end
		function a:infix(i, v, Q)
			if v == "OPR_AND" then
				self:goiftrue(i, Q)
			elseif v == "OPR_OR" then
				self:goiffalse(i, Q)
			elseif v == "OPR_CONCAT" then
				self:exp2nextreg(i, Q)
			elseif
				v == "OPR_ADD"
				or v == "OPR_SUB"
				or v == "OPR_MUL"
				or v == "OPR_DIV"
				or v == "OPR_MOD"
				or v == "OPR_POW"
			then
				if not self:isnumeral(Q) then
					self:exp2RK(i, Q)
				end
			else
				self:exp2RK(i, Q)
			end
		end
		a.arith_op = {
			OPR_ADD = "OP_ADD",
			OPR_SUB = "OP_SUB",
			OPR_MUL = "OP_MUL",
			OPR_DIV = "OP_DIV",
			OPR_MOD = "OP_MOD",
			OPR_POW = "OP_POW",
		}
		a.comp_op = {
			OPR_EQ = "OP_EQ",
			OPR_NE = "OP_EQ",
			OPR_LT = "OP_LT",
			OPR_LE = "OP_LE",
			OPR_GT = "OP_LT",
			OPR_GE = "OP_LE",
		}
		a.comp_cond = { OPR_EQ = 1, OPR_NE = 0, OPR_LT = 1, OPR_LE = 1, OPR_GT = 0, OPR_GE = 0 }
		function a:posfix(i, v, a9, aa)
			local function af(a9, aa)
				a9.k = aa.k
				a9.info = aa.info
				a9.aux = aa.aux
				a9.nval = aa.nval
				a9.t = aa.t
				a9.f = aa.f
			end
			if v == "OPR_AND" then
				assert(a9.t == self.NO_JUMP)
				self:dischargevars(i, aa)
				aa.f = self:concat(i, aa.f, a9.f)
				af(a9, aa)
			elseif v == "OPR_OR" then
				assert(a9.f == self.NO_JUMP)
				self:dischargevars(i, aa)
				aa.t = self:concat(i, aa.t, a9.t)
				af(a9, aa)
			elseif v == "OPR_CONCAT" then
				self:exp2val(i, aa)
				if aa.k == "VRELOCABLE" and c:GET_OPCODE(self:getcode(i, aa)) == "OP_CONCAT" then
					assert(a9.info == c:GETARG_B(self:getcode(i, aa)) - 1)
					self:freeexp(i, a9)
					c:SETARG_B(self:getcode(i, aa), a9.info)
					a9.k = "VRELOCABLE"
					a9.info = aa.info
				else
					self:exp2nextreg(i, aa)
					self:codearith(i, "OP_CONCAT", a9, aa)
				end
			else
				local ag = self.arith_op[v]
				if ag then
					self:codearith(i, ag, a9, aa)
				else
					local ah = self.comp_op[v]
					if ah then
						self:codecomp(i, ah, self.comp_cond[v], a9, aa)
					else
						assert(0)
					end
				end
			end
		end
		function a:fixline(i, ai)
			i.f.lineinfo[i.pc - 1] = ai
		end
		function a:code(i, F, ai)
			local T = i.f
			self:dischargejpc(i)
			LuaY:growvector(i.L, T.code, i.pc, T.sizecode, nil, LuaY.MAX_INT, "code size overflow")
			T.code[i.pc] = F
			LuaY:growvector(i.L, T.lineinfo, i.pc, T.sizelineinfo, nil, LuaY.MAX_INT, "code size overflow")
			T.lineinfo[i.pc] = ai
			local y = i.pc
			i.pc = i.pc + 1
			return y
		end
		function a:codeABC(i, e, g, h, aj)
			assert(c:getOpMode(e) == c.OpMode.iABC)
			assert(c:getBMode(e) ~= c.OpArgMask.OpArgN or h == 0)
			assert(c:getCMode(e) ~= c.OpArgMask.OpArgN or aj == 0)
			return self:code(i, c:CREATE_ABC(e, g, h, aj), i.ls.lastline)
		end
		function a:codeABx(i, e, g, ak)
			assert(c:getOpMode(e) == c.OpMode.iABx or c:getOpMode(e) == c.OpMode.iAsBx)
			assert(c:getCMode(e) == c.OpArgMask.OpArgN)
			return self:code(i, c:CREATE_ABx(e, g, ak), i.ls.lastline)
		end
		function a:setlist(i, al, am, an)
			local aj = math.floor((am - 1) / c.LFIELDS_PER_FLUSH) + 1
			local h = an == LuaY.LUA_MULTRET and 0 or an
			assert(an ~= 0)
			if aj <= c.MAXARG_C then
				self:codeABC(i, "OP_SETLIST", al, h, aj)
			else
				self:codeABC(i, "OP_SETLIST", al, h, 0)
				self:code(i, c:CREATE_Inst(aj), i.ls.lastline)
			end
			i.freereg = al + 1
		end
		LuaK = function(g)
			LuaY = g
			return a
		end
	end

	do -- LuaY Module:
		local a = {}
		local c = LuaX
		local d = LuaK(a)
		local e = LuaP
		a.LUA_QS = c.LUA_QS or "'%s'"
		a.SHRT_MAX = 32767
		a.LUAI_MAXVARS = 200
		a.LUAI_MAXUPVALUES = 60
		a.MAX_INT = c.MAX_INT or 2147483645
		a.LUAI_MAXCCALLS = 200
		a.VARARG_HASARG = 1
		a.HASARG_MASK = 2
		a.VARARG_ISVARARG = 2
		a.VARARG_NEEDSARG = 4
		a.LUA_MULTRET = -1
		function a:LUA_QL(f)
			return "'" .. f .. "'"
		end
		function a:growvector(g, h, i, j, k, l, m)
			if i >= l then
				error(m)
			end
		end
		function a:newproto(g)
			local n = {}
			n.k = {}
			n.sizek = 0
			n.p = {}
			n.sizep = 0
			n.code = {}
			n.sizecode = 0
			n.sizelineinfo = 0
			n.sizeupvalues = 0
			n.nups = 0
			n.upvalues = {}
			n.numparams = 0
			n.is_vararg = 0
			n.maxstacksize = 0
			n.lineinfo = {}
			n.sizelocvars = 0
			n.locvars = {}
			n.lineDefined = 0
			n.lastlinedefined = 0
			n.source = nil
			return n
		end
		function a:int2fb(f)
			local m = 0
			while f >= 16 do
				f = math.floor((f + 1) / 2)
				m = m + 1
			end
			if f < 8 then
				return f
			else
				return (m + 1) * 8 + f - 8
			end
		end
		function a:hasmultret(o)
			return o == "VCALL" or o == "VVARARG"
		end
		function a:getlocvar(p, q)
			return p.f.locvars[p.actvar[q]]
		end
		function a:checklimit(p, h, r, s)
			if h > r then
				self:errorlimit(p, r, s)
			end
		end
		function a:anchor_token(t)
			if t.t.token == "TK_NAME" or t.t.token == "TK_STRING" then
			end
		end
		function a:error_expected(t, u)
			c:syntaxerror(t, string.format(self.LUA_QS .. " expected", c:token2str(t, u)))
		end
		function a:errorlimit(p, l, v)
			local w = p.f.linedefined == 0 and string.format("main function has more than %d %s", l, v)
				or string.format("function at line %d has more than %d %s", p.f.linedefined, l, v)
			c:lexerror(p.ls, w, 0)
		end
		function a:testnext(t, x)
			if t.t.token == x then
				c:next(t)
				return true
			else
				return false
			end
		end
		function a:check(t, x)
			if t.t.token ~= x then
				self:error_expected(t, x)
			end
		end
		function a:checknext(t, x)
			self:check(t, x)
			c:next(t)
		end
		function a:check_condition(t, x, w)
			if not x then
				c:syntaxerror(t, w)
			end
		end
		function a:check_match(t, v, y, z)
			if not self:testnext(t, v) then
				if z == t.linenumber then
					self:error_expected(t, v)
				else
					c:syntaxerror(
						t,
						string.format(
							self.LUA_QS .. " expected (to close " .. self.LUA_QS .. " at line %d)",
							c:token2str(t, v),
							c:token2str(t, y),
							z
						)
					)
				end
			end
		end
		function a:str_checkname(t)
			self:check(t, "TK_NAME")
			local A = t.t.seminfo
			c:next(t)
			return A
		end
		function a:init_exp(m, o, q)
			m.f, m.t = d.NO_JUMP, d.NO_JUMP
			m.k = o
			m.info = q
		end
		function a:codestring(t, m, B)
			self:init_exp(m, "VK", d:stringK(t.fs, B))
		end
		function a:checkname(t, m)
			self:codestring(t, m, self:str_checkname(t))
		end
		function a:registerlocalvar(t, C)
			local p = t.fs
			local n = p.f
			self:growvector(t.L, n.locvars, p.nlocvars, n.sizelocvars, nil, self.SHRT_MAX, "too many local variables")
			n.locvars[p.nlocvars] = {}
			n.locvars[p.nlocvars].varname = C
			local D = p.nlocvars
			p.nlocvars = p.nlocvars + 1
			return D
		end
		function a:new_localvarliteral(t, h, E)
			self:new_localvar(t, h, E)
		end
		function a:new_localvar(t, F, E)
			local p = t.fs
			self:checklimit(p, p.nactvar + E + 1, self.LUAI_MAXVARS, "local variables")
			p.actvar[p.nactvar + E] = self:registerlocalvar(t, F)
		end
		function a:adjustlocalvars(t, G)
			local p = t.fs
			p.nactvar = p.nactvar + G
			for q = G, 1, -1 do
				self:getlocvar(p, p.nactvar - q).startpc = p.pc
			end
		end
		function a:removevars(t, H)
			local p = t.fs
			while p.nactvar > H do
				p.nactvar = p.nactvar - 1
				self:getlocvar(p, p.nactvar).endpc = p.pc
			end
		end
		function a:indexupvalue(p, F, h)
			local n = p.f
			for q = 0, n.nups - 1 do
				if p.upvalues[q].k == h.k and p.upvalues[q].info == h.info then
					assert(n.upvalues[q] == F)
					return q
				end
			end
			self:checklimit(p, n.nups + 1, self.LUAI_MAXUPVALUES, "upvalues")
			self:growvector(p.L, n.upvalues, n.nups, n.sizeupvalues, nil, self.MAX_INT, "")
			n.upvalues[n.nups] = F
			assert(h.k == "VLOCAL" or h.k == "VUPVAL")
			p.upvalues[n.nups] = { k = h.k, info = h.info }
			local I = n.nups
			n.nups = n.nups + 1
			return I
		end
		function a:searchvar(p, E)
			for q = p.nactvar - 1, 0, -1 do
				if E == self:getlocvar(p, q).varname then
					return q
				end
			end
			return -1
		end
		function a:markupval(p, J)
			local K = p.bl
			while K and K.nactvar > J do
				K = K.previous
			end
			if K then
				K.upval = true
			end
		end
		function a:singlevaraux(p, E, L, M)
			if p == nil then
				self:init_exp(L, "VGLOBAL", e.NO_REG)
				return "VGLOBAL"
			else
				local h = self:searchvar(p, E)
				if h >= 0 then
					self:init_exp(L, "VLOCAL", h)
					if M == 0 then
						self:markupval(p, h)
					end
					return "VLOCAL"
				else
					if self:singlevaraux(p.prev, E, L, 0) == "VGLOBAL" then
						return "VGLOBAL"
					end
					L.info = self:indexupvalue(p, E, L)
					L.k = "VUPVAL"
					return "VUPVAL"
				end
			end
		end
		function a:singlevar(t, L)
			local C = self:str_checkname(t)
			local p = t.fs
			if self:singlevaraux(p, C, L, 1) == "VGLOBAL" then
				L.info = d:stringK(p, C)
			end
		end
		function a:adjust_assign(t, G, N, m)
			local p = t.fs
			local O = G - N
			if self:hasmultret(m.k) then
				O = O + 1
				if O <= 0 then
					O = 0
				end
				d:setreturns(p, m, O)
				if O > 1 then
					d:reserveregs(p, O - 1)
				end
			else
				if m.k ~= "VVOID" then
					d:exp2nextreg(p, m)
				end
				if O > 0 then
					local P = p.freereg
					d:reserveregs(p, O)
					d:_nil(p, P, O)
				end
			end
		end
		function a:enterlevel(t)
			t.L.nCcalls = t.L.nCcalls + 1
			if t.L.nCcalls > self.LUAI_MAXCCALLS then
				c:lexerror(t, "chunk has too many syntax levels", 0)
			end
		end
		function a:leavelevel(t)
			t.L.nCcalls = t.L.nCcalls - 1
		end
		function a:enterblock(p, K, Q)
			K.breaklist = d.NO_JUMP
			K.isbreakable = Q
			K.nactvar = p.nactvar
			K.upval = false
			K.previous = p.bl
			p.bl = K
			assert(p.freereg == p.nactvar)
		end
		function a:leaveblock(p)
			local K = p.bl
			p.bl = K.previous
			self:removevars(p.ls, K.nactvar)
			if K.upval then
				d:codeABC(p, "OP_CLOSE", K.nactvar, 0, 0)
			end
			assert(not K.isbreakable or not K.upval)
			assert(K.nactvar == p.nactvar)
			p.freereg = p.nactvar
			d:patchtohere(p, K.breaklist)
		end
		function a:pushclosure(t, R, h)
			local p = t.fs
			local n = p.f
			self:growvector(t.L, n.p, p.np, n.sizep, nil, e.MAXARG_Bx, "constant table overflow")
			n.p[p.np] = R.f
			p.np = p.np + 1
			self:init_exp(h, "VRELOCABLE", d:codeABx(p, "OP_CLOSURE", 0, p.np - 1))
			for q = 0, R.f.nups - 1 do
				local S = R.upvalues[q].k == "VLOCAL" and "OP_MOVE" or "OP_GETUPVAL"
				d:codeABC(p, S, 0, R.upvalues[q].info, 0)
			end
		end
		function a:open_func(t, p)
			local g = t.L
			local n = self:newproto(t.L)
			p.f = n
			p.prev = t.fs
			p.ls = t
			p.L = g
			t.fs = p
			p.pc = 0
			p.lasttarget = -1
			p.jpc = d.NO_JUMP
			p.freereg = 0
			p.nk = 0
			p.np = 0
			p.nlocvars = 0
			p.nactvar = 0
			p.bl = nil
			n.source = t.source
			n.maxstacksize = 2
			p.h = {}
		end
		function a:close_func(t)
			local g = t.L
			local p = t.fs
			local n = p.f
			self:removevars(t, 0)
			d:ret(p, 0, 0)
			n.sizecode = p.pc
			n.sizelineinfo = p.pc
			n.sizek = p.nk
			n.sizep = p.np
			n.sizelocvars = p.nlocvars
			n.sizeupvalues = n.nups
			assert(p.bl == nil)
			t.fs = p.prev
			if p then
				self:anchor_token(t)
			end
		end
		function a:parser(g, T, U, F)
			local V = {}
			V.t = {}
			V.lookahead = {}
			local W = {}
			W.upvalues = {}
			W.actvar = {}
			g.nCcalls = 0
			V.buff = U
			c:setinput(g, V, T, F)
			self:open_func(V, W)
			W.f.is_vararg = self.VARARG_ISVARARG
			c:next(V)
			self:chunk(V)
			self:check(V, "TK_EOS")
			self:close_func(V)
			assert(W.prev == nil)
			assert(W.f.nups == 0)
			assert(V.fs == nil)
			return W.f
		end
		function a:field(t, h)
			local p = t.fs
			local X = {}
			d:exp2anyreg(p, h)
			c:next(t)
			self:checkname(t, X)
			d:indexed(p, h, X)
		end
		function a:yindex(t, h)
			c:next(t)
			self:expr(t, h)
			d:exp2val(t.fs, h)
			self:checknext(t, "]")
		end
		function a:recfield(t, Y)
			local p = t.fs
			local P = t.fs.freereg
			local X, Z = {}, {}
			if t.t.token == "TK_NAME" then
				self:checklimit(p, Y.nh, self.MAX_INT, "items in a constructor")
				self:checkname(t, X)
			else
				self:yindex(t, X)
			end
			Y.nh = Y.nh + 1
			self:checknext(t, "=")
			local _ = d:exp2RK(p, X)
			self:expr(t, Z)
			d:codeABC(p, "OP_SETTABLE", Y.t.info, _, d:exp2RK(p, Z))
			p.freereg = P
		end
		function a:closelistfield(p, Y)
			if Y.v.k == "VVOID" then
				return
			end
			d:exp2nextreg(p, Y.v)
			Y.v.k = "VVOID"
			if Y.tostore == e.LFIELDS_PER_FLUSH then
				d:setlist(p, Y.t.info, Y.na, Y.tostore)
				Y.tostore = 0
			end
		end
		function a:lastlistfield(p, Y)
			if Y.tostore == 0 then
				return
			end
			if self:hasmultret(Y.v.k) then
				d:setmultret(p, Y.v)
				d:setlist(p, Y.t.info, Y.na, self.LUA_MULTRET)
				Y.na = Y.na - 1
			else
				if Y.v.k ~= "VVOID" then
					d:exp2nextreg(p, Y.v)
				end
				d:setlist(p, Y.t.info, Y.na, Y.tostore)
			end
		end
		function a:listfield(t, Y)
			self:expr(t, Y.v)
			self:checklimit(t.fs, Y.na, self.MAX_INT, "items in a constructor")
			Y.na = Y.na + 1
			Y.tostore = Y.tostore + 1
		end
		function a:constructor(t, k)
			local p = t.fs
			local a0 = t.linenumber
			local a1 = d:codeABC(p, "OP_NEWTABLE", 0, 0, 0)
			local Y = {}
			Y.v = {}
			Y.na, Y.nh, Y.tostore = 0, 0, 0
			Y.t = k
			self:init_exp(k, "VRELOCABLE", a1)
			self:init_exp(Y.v, "VVOID", 0)
			d:exp2nextreg(t.fs, k)
			self:checknext(t, "{")
			repeat
				assert(Y.v.k == "VVOID" or Y.tostore > 0)
				if t.t.token == "}" then
					break
				end
				self:closelistfield(p, Y)
				local x = t.t.token
				if x == "TK_NAME" then
					c:lookahead(t)
					if t.lookahead.token ~= "=" then
						self:listfield(t, Y)
					else
						self:recfield(t, Y)
					end
				elseif x == "[" then
					self:recfield(t, Y)
				else
					self:listfield(t, Y)
				end
			until not self:testnext(t, ",") and not self:testnext(t, ";")
			self:check_match(t, "}", "{", a0)
			self:lastlistfield(p, Y)
			e:SETARG_B(p.f.code[a1], self:int2fb(Y.na))
			e:SETARG_C(p.f.code[a1], self:int2fb(Y.nh))
		end
		function a:parlist(t)
			local p = t.fs
			local n = p.f
			local a2 = 0
			n.is_vararg = 0
			if t.t.token ~= ")" then
				repeat
					local x = t.t.token
					if x == "TK_NAME" then
						self:new_localvar(t, self:str_checkname(t), a2)
						a2 = a2 + 1
					elseif x == "TK_DOTS" then
						c:next(t)
						self:new_localvarliteral(t, "arg", a2)
						a2 = a2 + 1
						n.is_vararg = self.VARARG_HASARG + self.VARARG_NEEDSARG
						n.is_vararg = n.is_vararg + self.VARARG_ISVARARG
					else
						c:syntaxerror(t, "<name> or " .. self:LUA_QL("...") .. " expected")
					end
				until n.is_vararg ~= 0 or not self:testnext(t, ",")
			end
			self:adjustlocalvars(t, a2)
			n.numparams = p.nactvar - n.is_vararg % self.HASARG_MASK
			d:reserveregs(p, p.nactvar)
		end
		function a:body(t, m, a3, a0)
			local a4 = {}
			a4.upvalues = {}
			a4.actvar = {}
			self:open_func(t, a4)
			a4.f.lineDefined = a0
			self:checknext(t, "(")
			if a3 then
				self:new_localvarliteral(t, "self", 0)
				self:adjustlocalvars(t, 1)
			end
			self:parlist(t)
			self:checknext(t, ")")
			self:chunk(t)
			a4.f.lastlinedefined = t.linenumber
			self:check_match(t, "TK_END", "TK_FUNCTION", a0)
			self:close_func(t)
			self:pushclosure(t, a4, m)
		end
		function a:explist1(t, h)
			local E = 1
			self:expr(t, h)
			while self:testnext(t, ",") do
				d:exp2nextreg(t.fs, h)
				self:expr(t, h)
				E = E + 1
			end
			return E
		end
		function a:funcargs(t, n)
			local p = t.fs
			local a5 = {}
			local a2
			local a0 = t.linenumber
			local x = t.t.token
			if x == "(" then
				if a0 ~= t.lastline then
					c:syntaxerror(t, "ambiguous syntax (function call x new statement)")
				end
				c:next(t)
				if t.t.token == ")" then
					a5.k = "VVOID"
				else
					self:explist1(t, a5)
					d:setmultret(p, a5)
				end
				self:check_match(t, ")", "(", a0)
			elseif x == "{" then
				self:constructor(t, a5)
			elseif x == "TK_STRING" then
				self:codestring(t, a5, t.t.seminfo)
				c:next(t)
			else
				c:syntaxerror(t, "function arguments expected")
				return
			end
			assert(n.k == "VNONRELOC")
			local M = n.info
			if self:hasmultret(a5.k) then
				a2 = self.LUA_MULTRET
			else
				if a5.k ~= "VVOID" then
					d:exp2nextreg(p, a5)
				end
				a2 = p.freereg - (M + 1)
			end
			self:init_exp(n, "VCALL", d:codeABC(p, "OP_CALL", M, a2 + 1, 2))
			d:fixline(p, a0)
			p.freereg = M + 1
		end
		function a:prefixexp(t, h)
			local x = t.t.token
			if x == "(" then
				local a0 = t.linenumber
				c:next(t)
				self:expr(t, h)
				self:check_match(t, ")", "(", a0)
				d:dischargevars(t.fs, h)
			elseif x == "TK_NAME" then
				self:singlevar(t, h)
			else
				c:syntaxerror(t, "unexpected symbol")
			end
			return
		end
		function a:primaryexp(t, h)
			local p = t.fs
			self:prefixexp(t, h)
			while true do
				local x = t.t.token
				if x == "." then
					self:field(t, h)
				elseif x == "[" then
					local X = {}
					d:exp2anyreg(p, h)
					self:yindex(t, X)
					d:indexed(p, h, X)
				elseif x == ":" then
					local X = {}
					c:next(t)
					self:checkname(t, X)
					d:_self(p, h, X)
					self:funcargs(t, h)
				elseif x == "(" or x == "TK_STRING" or x == "{" then
					d:exp2nextreg(p, h)
					self:funcargs(t, h)
				else
					return
				end
			end
		end
		function a:simpleexp(t, h)
			local x = t.t.token
			if x == "TK_NUMBER" then
				self:init_exp(h, "VKNUM", 0)
				h.nval = t.t.seminfo
			elseif x == "TK_STRING" then
				self:codestring(t, h, t.t.seminfo)
			elseif x == "TK_NIL" then
				self:init_exp(h, "VNIL", 0)
			elseif x == "TK_TRUE" then
				self:init_exp(h, "VTRUE", 0)
			elseif x == "TK_FALSE" then
				self:init_exp(h, "VFALSE", 0)
			elseif x == "TK_DOTS" then
				local p = t.fs
				self:check_condition(
					t,
					p.f.is_vararg ~= 0,
					"cannot use " .. self:LUA_QL("...") .. " outside a vararg function"
				)
				local a6 = p.f.is_vararg
				if a6 >= self.VARARG_NEEDSARG then
					p.f.is_vararg = a6 - self.VARARG_NEEDSARG
				end
				self:init_exp(h, "VVARARG", d:codeABC(p, "OP_VARARG", 0, 1, 0))
			elseif x == "{" then
				self:constructor(t, h)
				return
			elseif x == "TK_FUNCTION" then
				c:next(t)
				self:body(t, h, false, t.linenumber)
				return
			else
				self:primaryexp(t, h)
				return
			end
			c:next(t)
		end
		function a:getunopr(a7)
			if a7 == "TK_NOT" then
				return "OPR_NOT"
			elseif a7 == "-" then
				return "OPR_MINUS"
			elseif a7 == "#" then
				return "OPR_LEN"
			else
				return "OPR_NOUNOPR"
			end
		end
		a.getbinopr_table = {
			["+"] = "OPR_ADD",
			["-"] = "OPR_SUB",
			["*"] = "OPR_MUL",
			["/"] = "OPR_DIV",
			["%"] = "OPR_MOD",
			["^"] = "OPR_POW",
			["TK_CONCAT"] = "OPR_CONCAT",
			["TK_NE"] = "OPR_NE",
			["TK_EQ"] = "OPR_EQ",
			["<"] = "OPR_LT",
			["TK_LE"] = "OPR_LE",
			[">"] = "OPR_GT",
			["TK_GE"] = "OPR_GE",
			["TK_AND"] = "OPR_AND",
			["TK_OR"] = "OPR_OR",
		}
		function a:getbinopr(a7)
			local a8 = self.getbinopr_table[a7]
			if a8 then
				return a8
			else
				return "OPR_NOBINOPR"
			end
		end
		a.priority = {
			{ 6, 6 },
			{ 6, 6 },
			{ 7, 7 },
			{ 7, 7 },
			{ 7, 7 },
			{ 10, 9 },
			{ 5, 4 },
			{ 3, 3 },
			{ 3, 3 },
			{ 3, 3 },
			{ 3, 3 },
			{ 3, 3 },
			{ 3, 3 },
			{ 2, 2 },
			{ 1, 1 },
		}
		a.UNARY_PRIORITY = 8
		function a:subexpr(t, h, l)
			self:enterlevel(t)
			local a9 = self:getunopr(t.t.token)
			if a9 ~= "OPR_NOUNOPR" then
				c:next(t)
				self:subexpr(t, h, self.UNARY_PRIORITY)
				d:prefix(t.fs, a9, h)
			else
				self:simpleexp(t, h)
			end
			local a7 = self:getbinopr(t.t.token)
			while a7 ~= "OPR_NOBINOPR" and self.priority[d.BinOpr[a7] + 1][1] > l do
				local aa = {}
				c:next(t)
				d:infix(t.fs, a7, h)
				local ab = self:subexpr(t, aa, self.priority[d.BinOpr[a7] + 1][2])
				d:posfix(t.fs, a7, h, aa)
				a7 = ab
			end
			self:leavelevel(t)
			return a7
		end
		function a:expr(t, h)
			self:subexpr(t, h, 0)
		end
		function a:block_follow(u)
			if u == "TK_ELSE" or u == "TK_ELSEIF" or u == "TK_END" or u == "TK_UNTIL" or u == "TK_EOS" then
				return true
			else
				return false
			end
		end
		function a:block(t)
			local p = t.fs
			local K = {}
			self:enterblock(p, K, false)
			self:chunk(t)
			assert(K.breaklist == d.NO_JUMP)
			self:leaveblock(p)
		end
		function a:check_conflict(t, ac, h)
			local p = t.fs
			local O = p.freereg
			local ad = false
			while ac do
				if ac.v.k == "VINDEXED" then
					if ac.v.info == h.info then
						ad = true
						ac.v.info = O
					end
					if ac.v.aux == h.info then
						ad = true
						ac.v.aux = O
					end
				end
				ac = ac.prev
			end
			if ad then
				d:codeABC(p, "OP_MOVE", p.freereg, h.info, 0)
				d:reserveregs(p, 1)
			end
		end
		function a:assignment(t, ac, G)
			local m = {}
			local x = ac.v.k
			self:check_condition(t, x == "VLOCAL" or x == "VUPVAL" or x == "VGLOBAL" or x == "VINDEXED", "syntax error")
			if self:testnext(t, ",") then
				local ae = {}
				ae.v = {}
				ae.prev = ac
				self:primaryexp(t, ae.v)
				if ae.v.k == "VLOCAL" then
					self:check_conflict(t, ac, ae.v)
				end
				self:checklimit(t.fs, G, self.LUAI_MAXCCALLS - t.L.nCcalls, "variables in assignment")
				self:assignment(t, ae, G + 1)
			else
				self:checknext(t, "=")
				local N = self:explist1(t, m)
				if N ~= G then
					self:adjust_assign(t, G, N, m)
					if N > G then
						t.fs.freereg = t.fs.freereg - (N - G)
					end
				else
					d:setoneret(t.fs, m)
					d:storevar(t.fs, ac.v, m)
					return
				end
			end
			self:init_exp(m, "VNONRELOC", t.fs.freereg - 1)
			d:storevar(t.fs, ac.v, m)
		end
		function a:cond(t)
			local h = {}
			self:expr(t, h)
			if h.k == "VNIL" then
				h.k = "VFALSE"
			end
			d:goiftrue(t.fs, h)
			return h.f
		end
		function a:breakstat(t)
			local p = t.fs
			local K = p.bl
			local af = false
			while K and not K.isbreakable do
				if K.upval then
					af = true
				end
				K = K.previous
			end
			if not K then
				c:syntaxerror(t, "no loop to break")
			end
			if af then
				d:codeABC(p, "OP_CLOSE", K.nactvar, 0, 0)
			end
			K.breaklist = d:concat(p, K.breaklist, d:jump(p))
		end
		function a:whilestat(t, a0)
			local p = t.fs
			local K = {}
			c:next(t)
			local ag = d:getlabel(p)
			local ah = self:cond(t)
			self:enterblock(p, K, true)
			self:checknext(t, "TK_DO")
			self:block(t)
			d:patchlist(p, d:jump(p), ag)
			self:check_match(t, "TK_END", "TK_WHILE", a0)
			self:leaveblock(p)
			d:patchtohere(p, ah)
		end
		function a:repeatstat(t, a0)
			local p = t.fs
			local ai = d:getlabel(p)
			local aj, ak = {}, {}
			self:enterblock(p, aj, true)
			self:enterblock(p, ak, false)
			c:next(t)
			self:chunk(t)
			self:check_match(t, "TK_UNTIL", "TK_REPEAT", a0)
			local ah = self:cond(t)
			if not ak.upval then
				self:leaveblock(p)
				d:patchlist(t.fs, ah, ai)
			else
				self:breakstat(t)
				d:patchtohere(t.fs, ah)
				self:leaveblock(p)
				d:patchlist(t.fs, d:jump(p), ai)
			end
			self:leaveblock(p)
		end
		function a:exp1(t)
			local m = {}
			self:expr(t, m)
			local o = m.k
			d:exp2nextreg(t.fs, m)
			return o
		end
		function a:forbody(t, M, a0, G, al)
			local K = {}
			local p = t.fs
			self:adjustlocalvars(t, 3)
			self:checknext(t, "TK_DO")
			local am = al and d:codeAsBx(p, "OP_FORPREP", M, d.NO_JUMP) or d:jump(p)
			self:enterblock(p, K, false)
			self:adjustlocalvars(t, G)
			d:reserveregs(p, G)
			self:block(t)
			self:leaveblock(p)
			d:patchtohere(p, am)
			local an = al and d:codeAsBx(p, "OP_FORLOOP", M, d.NO_JUMP) or d:codeABC(p, "OP_TFORLOOP", M, 0, G)
			d:fixline(p, a0)
			d:patchlist(p, al and an or d:jump(p), am + 1)
		end
		function a:fornum(t, C, a0)
			local p = t.fs
			local M = p.freereg
			self:new_localvarliteral(t, "(for index)", 0)
			self:new_localvarliteral(t, "(for limit)", 1)
			self:new_localvarliteral(t, "(for step)", 2)
			self:new_localvar(t, C, 3)
			self:checknext(t, "=")
			self:exp1(t)
			self:checknext(t, ",")
			self:exp1(t)
			if self:testnext(t, ",") then
				self:exp1(t)
			else
				d:codeABx(p, "OP_LOADK", p.freereg, d:numberK(p, 1))
				d:reserveregs(p, 1)
			end
			self:forbody(t, M, a0, 1, true)
		end
		function a:forlist(t, ao)
			local p = t.fs
			local m = {}
			local G = 0
			local M = p.freereg
			self:new_localvarliteral(t, "(for generator)", G)
			G = G + 1
			self:new_localvarliteral(t, "(for state)", G)
			G = G + 1
			self:new_localvarliteral(t, "(for control)", G)
			G = G + 1
			self:new_localvar(t, ao, G)
			G = G + 1
			while self:testnext(t, ",") do
				self:new_localvar(t, self:str_checkname(t), G)
				G = G + 1
			end
			self:checknext(t, "TK_IN")
			local a0 = t.linenumber
			self:adjust_assign(t, 3, self:explist1(t, m), m)
			d:checkstack(p, 3)
			self:forbody(t, M, a0, G - 3, false)
		end
		function a:forstat(t, a0)
			local p = t.fs
			local K = {}
			self:enterblock(p, K, true)
			c:next(t)
			local C = self:str_checkname(t)
			local x = t.t.token
			if x == "=" then
				self:fornum(t, C, a0)
			elseif x == "," or x == "TK_IN" then
				self:forlist(t, C)
			else
				c:syntaxerror(t, self:LUA_QL("=") .. " or " .. self:LUA_QL("in") .. " expected")
			end
			self:check_match(t, "TK_END", "TK_FOR", a0)
			self:leaveblock(p)
		end
		function a:test_then_block(t)
			c:next(t)
			local ah = self:cond(t)
			self:checknext(t, "TK_THEN")
			self:block(t)
			return ah
		end
		function a:ifstat(t, a0)
			local p = t.fs
			local ap = d.NO_JUMP
			local aq = self:test_then_block(t)
			while t.t.token == "TK_ELSEIF" do
				ap = d:concat(p, ap, d:jump(p))
				d:patchtohere(p, aq)
				aq = self:test_then_block(t)
			end
			if t.t.token == "TK_ELSE" then
				ap = d:concat(p, ap, d:jump(p))
				d:patchtohere(p, aq)
				c:next(t)
				self:block(t)
			else
				ap = d:concat(p, ap, aq)
			end
			d:patchtohere(p, ap)
			self:check_match(t, "TK_END", "TK_IF", a0)
		end
		function a:localfunc(t)
			local h, ar = {}, {}
			local p = t.fs
			self:new_localvar(t, self:str_checkname(t), 0)
			self:init_exp(h, "VLOCAL", p.freereg)
			d:reserveregs(p, 1)
			self:adjustlocalvars(t, 1)
			self:body(t, ar, false, t.linenumber)
			d:storevar(p, h, ar)
			self:getlocvar(p, p.nactvar - 1).startpc = p.pc
		end
		function a:localstat(t)
			local G = 0
			local N
			local m = {}
			repeat
				self:new_localvar(t, self:str_checkname(t), G)
				G = G + 1
			until not self:testnext(t, ",")
			if self:testnext(t, "=") then
				N = self:explist1(t, m)
			else
				m.k = "VVOID"
				N = 0
			end
			self:adjust_assign(t, G, N, m)
			self:adjustlocalvars(t, G)
		end
		function a:funcname(t, h)
			local a3 = false
			self:singlevar(t, h)
			while t.t.token == "." do
				self:field(t, h)
			end
			if t.t.token == ":" then
				a3 = true
				self:field(t, h)
			end
			return a3
		end
		function a:funcstat(t, a0)
			local h, ar = {}, {}
			c:next(t)
			local a3 = self:funcname(t, h)
			self:body(t, ar, a3, a0)
			d:storevar(t.fs, h, ar)
			d:fixline(t.fs, a0)
		end
		function a:exprstat(t)
			local p = t.fs
			local h = {}
			h.v = {}
			self:primaryexp(t, h.v)
			if h.v.k == "VCALL" then
				e:SETARG_C(d:getcode(p, h.v), 1)
			else
				h.prev = nil
				self:assignment(t, h, 1)
			end
		end
		function a:retstat(t)
			local p = t.fs
			local m = {}
			local as, at
			c:next(t)
			if self:block_follow(t.t.token) or t.t.token == ";" then
				as, at = 0, 0
			else
				at = self:explist1(t, m)
				if self:hasmultret(m.k) then
					d:setmultret(p, m)
					if m.k == "VCALL" and at == 1 then
						e:SET_OPCODE(d:getcode(p, m), "OP_TAILCALL")
						assert(e:GETARG_A(d:getcode(p, m)) == p.nactvar)
					end
					as = p.nactvar
					at = self.LUA_MULTRET
				else
					if at == 1 then
						as = d:exp2anyreg(p, m)
					else
						d:exp2nextreg(p, m)
						as = p.nactvar
						assert(at == p.freereg - as)
					end
				end
			end
			d:ret(p, as, at)
		end
		function a:statement(t)
			local a0 = t.linenumber
			local x = t.t.token
			if x == "TK_IF" then
				self:ifstat(t, a0)
				return false
			elseif x == "TK_WHILE" then
				self:whilestat(t, a0)
				return false
			elseif x == "TK_DO" then
				c:next(t)
				self:block(t)
				self:check_match(t, "TK_END", "TK_DO", a0)
				return false
			elseif x == "TK_FOR" then
				self:forstat(t, a0)
				return false
			elseif x == "TK_REPEAT" then
				self:repeatstat(t, a0)
				return false
			elseif x == "TK_FUNCTION" then
				self:funcstat(t, a0)
				return false
			elseif x == "TK_LOCAL" then
				c:next(t)
				if self:testnext(t, "TK_FUNCTION") then
					self:localfunc(t)
				else
					self:localstat(t)
				end
				return false
			elseif x == "TK_RETURN" then
				self:retstat(t)
				return true
			elseif x == "TK_BREAK" then
				c:next(t)
				self:breakstat(t)
				return true
			else
				self:exprstat(t)
				return false
			end
		end
		function a:chunk(t)
			local au = false
			self:enterlevel(t)
			while not au and not self:block_follow(t.t.token) do
				au = self:statement(t)
				self:testnext(t, ";")
				assert(t.fs.f.maxstacksize >= t.fs.freereg and t.fs.freereg >= t.fs.nactvar)
				t.fs.freereg = t.fs.nactvar
			end
			self:leavelevel(t)
		end
		LuaY = a
	end

	do -- LuaU Module:
		local a = {}
		local c = LuaP
		a.LUA_SIGNATURE = "\27Lua"
		a.LUA_TNUMBER = 3
		a.LUA_TSTRING = 4
		a.LUA_TNIL = 0
		a.LUA_TBOOLEAN = 1
		a.LUA_TNONE = -1
		a.LUAC_VERSION = 0x51
		a.LUAC_FORMAT = 0
		a.LUAC_HEADERSIZE = 12
		function a:make_setS()
			local d = {}
			d.data = ""
			local e = function(f, d)
				if not f then
					return 0
				end
				d.data = d.data .. f
				return 0
			end
			return e, d
		end
		function a:ttype(g)
			local h = type(g.value)
			if h == "number" then
				return self.LUA_TNUMBER
			elseif h == "string" then
				return self.LUA_TSTRING
			elseif h == "nil" then
				return self.LUA_TNIL
			elseif h == "boolean" then
				return self.LUA_TBOOLEAN
			else
				return self.LUA_TNONE
			end
		end
		function a:from_double(i)
			local function j(k)
				local l = k % 256
				return (k - l) / 256, string.char(l)
			end
			local m = 0
			if i < 0 then
				m = 1
				i = -i
			end
			local n, o = math.frexp(i)
			if i == 0 then
				n, o = 0, 0
			elseif i == 1 / 0 then
				n, o = 0, 2047
			else
				n = (n * 2 - 1) * math.ldexp(0.5, 53)
				o = o + 1022
			end
			local k, p = ""
			i = math.floor(n)
			for q = 1, 6 do
				i, p = j(i)
				k = k .. p
			end
			i, p = j(o * 16 + i)
			k = k .. p
			i, p = j(m * 128 + i)
			k = k .. p
			return k
		end
		function a:from_int(i)
			local k = ""
			i = math.floor(i)
			if i < 0 then
				i = 4294967296 + i
			end
			for q = 1, 4 do
				local l = i % 256
				k = k .. string.char(l)
				i = math.floor(i / 256)
			end
			return k
		end
		function a:DumpBlock(r, s)
			if s.status == 0 then
				s.status = s.write(r, s.data)
			end
		end
		function a:DumpChar(t, s)
			self:DumpBlock(string.char(t), s)
		end
		function a:DumpInt(i, s)
			self:DumpBlock(self:from_int(i), s)
		end
		function a:DumpNumber(i, s)
			self:DumpBlock(self:from_double(i), s)
		end
		function a:DumpString(f, s)
			if f == nil then
				self:DumpInt(0, s)
			else
				f = f .. "\0"
				self:DumpInt(#f, s)
				self:DumpBlock(f, s)
			end
		end
		function a:DumpCode(u, s)
			local v = u.sizecode
			self:DumpInt(v, s)
			for q = 0, v - 1 do
				self:DumpBlock(c:Instruction(u.code[q]), s)
			end
		end
		function a:DumpConstants(u, s)
			local v = u.sizek
			self:DumpInt(v, s)
			for q = 0, v - 1 do
				local g = u.k[q]
				local h = self:ttype(g)
				self:DumpChar(h, s)
				if h == self.LUA_TNIL then
				elseif h == self.LUA_TBOOLEAN then
					self:DumpChar(g.value and 1 or 0, s)
				elseif h == self.LUA_TNUMBER then
					self:DumpNumber(g.value, s)
				elseif h == self.LUA_TSTRING then
					self:DumpString(g.value, s)
				else
				end
			end
			v = u.sizep
			self:DumpInt(v, s)
			for q = 0, v - 1 do
				self:DumpFunction(u.p[q], u.source, s)
			end
		end
		function a:DumpDebug(u, s)
			local v
			v = s.strip and 0 or u.sizelineinfo
			self:DumpInt(v, s)
			for q = 0, v - 1 do
				self:DumpInt(u.lineinfo[q], s)
			end
			v = s.strip and 0 or u.sizelocvars
			self:DumpInt(v, s)
			for q = 0, v - 1 do
				self:DumpString(u.locvars[q].varname, s)
				self:DumpInt(u.locvars[q].startpc, s)
				self:DumpInt(u.locvars[q].endpc, s)
			end
			v = s.strip and 0 or u.sizeupvalues
			self:DumpInt(v, s)
			for q = 0, v - 1 do
				self:DumpString(u.upvalues[q], s)
			end
		end
		function a:DumpFunction(u, w, s)
			local x = u.source
			if x == w or s.strip then
				x = nil
			end
			self:DumpString(x, s)
			self:DumpInt(u.lineDefined, s)
			self:DumpInt(u.lastlinedefined, s)
			self:DumpChar(u.nups, s)
			self:DumpChar(u.numparams, s)
			self:DumpChar(u.is_vararg, s)
			self:DumpChar(u.maxstacksize, s)
			self:DumpCode(u, s)
			self:DumpConstants(u, s)
			self:DumpDebug(u, s)
		end
		function a:DumpHeader(s)
			local y = self:header()
			assert(#y == self.LUAC_HEADERSIZE)
			self:DumpBlock(y, s)
		end
		function a:header()
			local i = 1
			return self.LUA_SIGNATURE .. string.char(self.LUAC_VERSION, self.LUAC_FORMAT, i, 4, 4, 4, 8, 0)
		end
		function a:dump(z, u, A, B, C)
			local s = {}
			s.L = z
			s.write = A
			s.data = B
			s.strip = C
			s.status = 0
			self:DumpHeader(s)
			self:DumpFunction(u, nil, s)
			s.write(nil, s.data)
			return s.status
		end
		LuaU = a
	end

	do -- FiOne Module:
		local bit = bit32
		local lua_bc_to_state
		local lua_wrap_state
		local stm_lua_func

		local FIELDS_PER_FLUSH = 50

		local OPCODE_RM = {
			[22] = 18, -- JMP
			[31] = 8, -- FORLOOP
			[33] = 28, -- TFORLOOP

			[0] = 3, -- MOVE
			[1] = 13, -- LOADK
			[2] = 23, -- LOADBOOL
			[26] = 33, -- TEST

			[12] = 1, -- ADD
			[13] = 6, -- SUB
			[14] = 10, -- MUL
			[15] = 16, -- DIV
			[16] = 20, -- MOD
			[17] = 26, -- POW
			[18] = 30, -- UNM
			[19] = 36, -- NOT

			[3] = 0, -- LOADNIL
			[4] = 2, -- GETUPVAL
			[5] = 4, -- GETGLOBAL
			[6] = 7, -- GETTABLE
			[7] = 9, -- SETGLOBAL
			[8] = 12, -- SETUPVAL
			[9] = 14, -- SETTABLE
			[10] = 17, -- NEWTABLE
			[20] = 19, -- LEN
			[21] = 22, -- CONCAT
			[23] = 24, -- EQ
			[24] = 27, -- LT
			[25] = 29, -- LE
			[27] = 32, -- TESTSET
			[32] = 34, -- FORPREP
			[34] = 37, -- SETLIST

			[11] = 5, -- SELF
			[28] = 11, -- CALL
			[29] = 15, -- TAILCALL
			[30] = 21, -- RETURN
			[35] = 25, -- CLOSE
			[36] = 31, -- CLOSURE
			[37] = 35, -- VARARG
		}

		local OPCODE_T = {
			[0] = "ABC",
			"ABx",
			"ABC",
			"ABC",
			"ABC",
			"ABx",
			"ABC",
			"ABx",
			"ABC",
			"ABC",
			"ABC",
			"ABC",
			"ABC",
			"ABC",
			"ABC",
			"ABC",
			"ABC",
			"ABC",
			"ABC",
			"ABC",
			"ABC",
			"ABC",
			"AsBx",
			"ABC",
			"ABC",
			"ABC",
			"ABC",
			"ABC",
			"ABC",
			"ABC",
			"ABC",
			"AsBx",
			"AsBx",
			"ABC",
			"ABC",
			"ABC",
			"ABx",
			"ABC",
		}

		local OPCODE_M = {
			[0] = { b = "OpArgR", c = "OpArgN" },
			{ b = "OpArgK", c = "OpArgN" },
			{ b = "OpArgU", c = "OpArgU" },
			{ b = "OpArgR", c = "OpArgN" },
			{ b = "OpArgU", c = "OpArgN" },
			{ b = "OpArgK", c = "OpArgN" },
			{ b = "OpArgR", c = "OpArgK" },
			{ b = "OpArgK", c = "OpArgN" },
			{ b = "OpArgU", c = "OpArgN" },
			{ b = "OpArgK", c = "OpArgK" },
			{ b = "OpArgU", c = "OpArgU" },
			{ b = "OpArgR", c = "OpArgK" },
			{ b = "OpArgK", c = "OpArgK" },
			{ b = "OpArgK", c = "OpArgK" },
			{ b = "OpArgK", c = "OpArgK" },
			{ b = "OpArgK", c = "OpArgK" },
			{ b = "OpArgK", c = "OpArgK" },
			{ b = "OpArgK", c = "OpArgK" },
			{ b = "OpArgR", c = "OpArgN" },
			{ b = "OpArgR", c = "OpArgN" },
			{ b = "OpArgR", c = "OpArgN" },
			{ b = "OpArgR", c = "OpArgR" },
			{ b = "OpArgR", c = "OpArgN" },
			{ b = "OpArgK", c = "OpArgK" },
			{ b = "OpArgK", c = "OpArgK" },
			{ b = "OpArgK", c = "OpArgK" },
			{ b = "OpArgR", c = "OpArgU" },
			{ b = "OpArgR", c = "OpArgU" },
			{ b = "OpArgU", c = "OpArgU" },
			{ b = "OpArgU", c = "OpArgU" },
			{ b = "OpArgU", c = "OpArgN" },
			{ b = "OpArgR", c = "OpArgN" },
			{ b = "OpArgR", c = "OpArgN" },
			{ b = "OpArgN", c = "OpArgU" },
			{ b = "OpArgU", c = "OpArgU" },
			{ b = "OpArgN", c = "OpArgN" },
			{ b = "OpArgU", c = "OpArgN" },
			{ b = "OpArgU", c = "OpArgN" },
		}

		local op_index = "op"

		-- int rd_int_basic(string src, int s, int e, int d)
		-- @src - Source binary string
		-- @s - Start index of a little endian integer
		-- @e - End index of the integer
		-- @d - Direction of the loop
		local function rd_int_basic(src, s, e, d)
			local num = 0

			-- if bb[l] > 127 then -- signed negative
			-- 	num = num - 256 ^ l
			-- 	bb[l] = bb[l] - 128
			-- end

			for i = s, e, d do
				num = num + string.byte(src, i, i) * 256 ^ (i - s)
			end

			return num
		end

		-- float rd_flt_basic(byte f1..8)
		-- @f1..4 - The 4 bytes composing a little endian float
		local function rd_flt_basic(f1, f2, f3, f4)
			local sign = -1 ^ bit.rshift(f4, 7)
			local exp = bit.rshift(f3, 7) + bit.lshift(bit.band(f4, 0x7F), 1)
			local frac = f1 + bit.lshift(f2, 8) + bit.lshift(bit.band(f3, 0x7F), 16)
			local normal = 1

			if exp == 0 then
				if frac == 0 then
					return sign * 0
				else
					normal = 0
					exp = 1
				end
			elseif exp == 0x7F then
				if frac == 0 then
					return sign * (1 / 0)
				else
					return sign * (0 / 0)
				end
			end

			return sign * 2 ^ (exp - 127) * (1 + normal / 2 ^ 23)
		end

		-- double rd_dbl_basic(byte f1..8)
		-- @f1..8 - The 8 bytes composing a little endian double
		local function rd_dbl_basic(f1, f2, f3, f4, f5, f6, f7, f8)
			local sign = -1 ^ bit.rshift(f8, 7)
			local exp = bit.lshift(bit.band(f8, 0x7F), 4) + bit.rshift(f7, 4)
			local frac = bit.band(f7, 0x0F) * 2 ^ 48
			local normal = 1

			frac = frac + (f6 * 2 ^ 40) + (f5 * 2 ^ 32) + (f4 * 2 ^ 24) + (f3 * 2 ^ 16) + (f2 * 2 ^ 8) + f1 -- help

			if exp == 0 then
				if frac == 0 then
					return sign * 0
				else
					normal = 0
					exp = 1
				end
			elseif exp == 0x7FF then
				if frac == 0 then
					return sign * (1 / 0)
				else
					return sign * (0 / 0)
				end
			end

			return sign * 2 ^ (exp - 1023) * (normal + frac / 2 ^ 52)
		end

		local try = pcall

		-- int rd_int_le(string src, int s, int e)
		-- @src - Source binary string
		-- @s - Start index of a little endian integer
		-- @e - End index of the integer
		local function rd_int_le(src, s, e)
			return rd_int_basic(src, s, e - 1, 1)
		end

		-- int rd_int_be(string src, int s, int e)
		-- @src - Source binary string
		-- @s - Start index of a big endian integer
		-- @e - End index of the integer
		local function rd_int_be(src, s, e)
			return rd_int_basic(src, e - 1, s, -1)
		end

		-- float rd_flt_le(string src, int s)
		-- @src - Source binary string
		-- @s - Start index of little endian float
		local function rd_flt_le(src, s)
			return rd_flt_basic(string.byte(src, s, s + 3))
		end

		-- float rd_flt_be(string src, int s)
		-- @src - Source binary string
		-- @s - Start index of big endian float
		local function rd_flt_be(src, s)
			local f1, f2, f3, f4 = string.byte(src, s, s + 3)
			return rd_flt_basic(f4, f3, f2, f1)
		end

		-- double rd_dbl_le(string src, int s)
		-- @src - Source binary string
		-- @s - Start index of little endian double
		local function rd_dbl_le(src, s)
			return rd_dbl_basic(string.byte(src, s, s + 7))
		end

		-- double rd_dbl_be(string src, int s)
		-- @src - Source binary string
		-- @s - Start index of big endian double
		local function rd_dbl_be(src, s)
			local f1, f2, f3, f4, f5, f6, f7, f8 = string.byte(src, s, s + 7) -- same
			return rd_dbl_basic(f8, f7, f6, f5, f4, f3, f2, f1)
		end

		-- to avoid nested ifs in deserializing
		local float_types = {
			[4] = { little = rd_flt_le, big = rd_flt_be },
			[8] = { little = rd_dbl_le, big = rd_dbl_be },
		}

		-- byte stm_byte(Stream S)
		-- @S - Stream object to read from
		local function stm_byte(S)
			local idx = S.index
			local bt = string.byte(S.source, idx, idx)

			S.index = idx + 1
			return bt
		end

		-- string stm_string(Stream S, int len)
		-- @S - Stream object to read from
		-- @len - Length of string being read
		local function stm_string(S, len)
			local pos = S.index + len
			local str = string.sub(S.source, S.index, pos - 1)

			S.index = pos
			return str
		end

		-- string stm_lstring(Stream S)
		-- @S - Stream object to read from
		local function stm_lstring(S)
			local len = S:s_szt()
			local str

			if len ~= 0 then
				str = string.sub(stm_string(S, len), 1, -2)
			end

			return str
		end

		-- fn cst_int_rdr(string src, int len, fn func)
		-- @len - Length of type for reader
		-- @func - Reader callback
		local function cst_int_rdr(len, func)
			return function(S)
				local pos = S.index + len
				local int = func(S.source, S.index, pos)
				S.index = pos

				return int
			end
		end

		-- fn cst_flt_rdr(string src, int len, fn func)
		-- @len - Length of type for reader
		-- @func - Reader callback
		local function cst_flt_rdr(len, func)
			return function(S)
				local flt = func(S.source, S.index)
				S.index = S.index + len

				return flt
			end
		end

		local top_env

		local function stm_inst_list(S)
			local len = S:s_int()
			local list = table.create(len)

			for i = 1, len do
				local ins = S:s_ins()
				local op = bit.band(ins, 0x3F)
				local args = OPCODE_T[op]
				local mode = OPCODE_M[op]
				local data = { value = ins, op = OPCODE_RM[op], A = bit.band(bit.rshift(ins, 6), 0xFF) }

				if args == "ABC" then
					data.B = bit.band(bit.rshift(ins, 23), 0x1FF)
					data.C = bit.band(bit.rshift(ins, 14), 0x1FF)
					data.is_KB = mode.b == "OpArgK" and data.B > 0xFF -- post process optimization
					data.is_KC = mode.c == "OpArgK" and data.C > 0xFF
				elseif args == "ABx" then
					data.Bx = bit.band(bit.rshift(ins, 14), 0x3FFFF)
					data.is_K = mode.b == "OpArgK"
				elseif args == "AsBx" then
					data.sBx = bit.band(bit.rshift(ins, 14), 0x3FFFF) - 131071
				end

				list[i] = data
			end

			return list
		end

		local function stm_const_list(S)
			local len = S:s_int()
			local list = table.create(len)

			for i = 1, len do
				local tt = stm_byte(S)
				local k

				if tt == 1 then
					k = stm_byte(S) ~= 0
				elseif tt == 3 then
					k = S:s_num()
				elseif tt == 4 then
					k = stm_lstring(S)
				end

				list[i] = k -- offset +1 during instruction decode
			end

			return list
		end

		local function stm_sub_list(S, src)
			local len = S:s_int()
			local list = table.create(len)

			for i = 1, len do
				list[i] = stm_lua_func(S, src) -- offset +1 in CLOSURE
			end

			return list
		end

		local function stm_line_list(S)
			local len = S:s_int()
			local list = table.create(len)

			for i = 1, len do
				list[i] = S:s_int()
			end

			return list
		end

		local function stm_loc_list(S)
			local len = S:s_int()
			local list = table.create(len)

			for i = 1, len do
				list[i] = { varname = stm_lstring(S), startpc = S:s_int(), endpc = S:s_int() }
			end

			return list
		end

		local function stm_upval_list(S)
			local len = S:s_int()
			local list = table.create(len)

			for i = 1, len do
				list[i] = stm_lstring(S)
			end

			return list
		end

		function stm_lua_func(S, psrc)
			local proto = {}
			local src = stm_lstring(S) or psrc -- source is propagated

			proto.source = src -- source name

			S:s_int() -- line defined
			S:s_int() -- last line defined

			proto.num_upval = stm_byte(S) -- num upvalues
			proto.num_param = stm_byte(S) -- num params

			stm_byte(S) -- vararg flag
			proto.max_stack = stm_byte(S) -- max stack size

			proto.code = stm_inst_list(S)
			proto.const = stm_const_list(S)
			proto.subs = stm_sub_list(S, src)
			proto.lines = stm_line_list(S)

			stm_loc_list(S)
			stm_upval_list(S)

			-- post process optimization
			for _, v in ipairs(proto.code) do
				if v.is_K then
					v.const = proto.const[v.Bx + 1] -- offset for 1 based index
				else
					if v.is_KB then
						v.const_B = proto.const[v.B - 0xFF]
					end

					if v.is_KC then
						v.const_C = proto.const[v.C - 0xFF]
					end
				end
			end

			return proto
		end

		function lua_bc_to_state(src)
			-- func reader
			local rdr_func

			-- header flags
			local little
			local size_int
			local size_szt
			local size_ins
			local size_num
			local flag_int

			-- stream object
			local stream = {
				-- data
				index = 1,
				source = src,
			}

			assert(stm_string(stream, 4) == "\27Lua", "invalid Lua signature")
			assert(stm_byte(stream) == 0x51, "invalid Lua version")
			assert(stm_byte(stream) == 0, "invalid Lua format")

			little = stm_byte(stream) ~= 0
			size_int = stm_byte(stream)
			size_szt = stm_byte(stream)
			size_ins = stm_byte(stream)
			size_num = stm_byte(stream)
			flag_int = stm_byte(stream) ~= 0

			rdr_func = little and rd_int_le or rd_int_be
			stream.s_int = cst_int_rdr(size_int, rdr_func)
			stream.s_szt = cst_int_rdr(size_szt, rdr_func)
			stream.s_ins = cst_int_rdr(size_ins, rdr_func)

			if flag_int then
				stream.s_num = cst_int_rdr(size_num, rdr_func)
			elseif float_types[size_num] then
				stream.s_num = cst_flt_rdr(size_num, float_types[size_num][little and "little" or "big"])
			else
				error("unsupported float size")
			end

			return stm_lua_func(stream, "@virtual")
		end

		local function close_lua_upvalues(list, index)
			for i, uv in pairs(list) do
				if uv.index >= index then
					uv.value = uv.store[uv.index] -- store value
					uv.store = uv
					uv.index = "value" -- self reference
					list[i] = nil
				end
			end
		end

		local function open_lua_upvalue(list, index, memory)
			local prev = list[index]

			if not prev then
				prev = { index = index, store = memory }
				list[index] = prev
			end

			return prev
		end

		local function on_lua_error(failed, err)
			local src = failed.source
			local line = failed.lines[failed.pc - 1]

			error(string.format("%s:%i: %s", src, line, err), 0)
		end

		local function run_lua_func(state, env, upvals)
			local code = state.code
			local subs = state.subs
			local vararg = state.vararg

			local top_index = -1
			local open_list = {}
			local memory = state.memory
			local pc = state.pc

			while true do
				local inst = code[pc]
				local op = inst[op_index]
				pc = pc + 1

				if op < 18 then
					if op < 8 then
						if op < 3 then
							if op < 1 then
								--[[LOADNIL]]
								for i = inst.A, inst.B do
									memory[i] = nil
								end
							elseif op > 1 then
								--[[GETUPVAL]]
								local uv = upvals[inst.B]

								memory[inst.A] = uv.store[uv.index]
							else
								--[[ADD]]
								local lhs, rhs

								if inst.is_KB then
									lhs = inst.const_B
								else
									lhs = memory[inst.B]
								end

								if inst.is_KC then
									rhs = inst.const_C
								else
									rhs = memory[inst.C]
								end

								memory[inst.A] = lhs + rhs
							end
						elseif op > 3 then
							if op < 6 then
								if op > 4 then
									--[[SELF]]
									local A = inst.A
									local B = inst.B
									local index

									if inst.is_KC then
										index = inst.const_C
									else
										index = memory[inst.C]
									end

									memory[A + 1] = memory[B]
									memory[A] = memory[B][index]
								else
									--[[GETGLOBAL]]
									memory[inst.A] = env[inst.const]
								end
							elseif op > 6 then
								--[[GETTABLE]]
								local index

								if inst.is_KC then
									index = inst.const_C
								else
									index = memory[inst.C]
								end

								memory[inst.A] = memory[inst.B][index]
							else
								--[[SUB]]
								local lhs, rhs

								if inst.is_KB then
									lhs = inst.const_B
								else
									lhs = memory[inst.B]
								end

								if inst.is_KC then
									rhs = inst.const_C
								else
									rhs = memory[inst.C]
								end

								memory[inst.A] = lhs - rhs
							end
						else --[[MOVE]]
							memory[inst.A] = memory[inst.B]
						end
					elseif op > 8 then
						if op < 13 then
							if op < 10 then
								--[[SETGLOBAL]]
								env[inst.const] = memory[inst.A]
							elseif op > 10 then
								if op < 12 then
									--[[CALL]]
									local A = inst.A
									local B = inst.B
									local C = inst.C
									local params

									if B == 0 then
										params = top_index - A
									else
										params = B - 1
									end

									local ret_list = table.pack(memory[A](table.unpack(memory, A + 1, A + params)))
									local ret_num = ret_list.n

									if C == 0 then
										top_index = A + ret_num - 1
									else
										ret_num = C - 1
									end

									table.move(ret_list, 1, ret_num, A, memory)
								else
									--[[SETUPVAL]]
									local uv = upvals[inst.B]

									uv.store[uv.index] = memory[inst.A]
								end
							else
								--[[MUL]]
								local lhs, rhs

								if inst.is_KB then
									lhs = inst.const_B
								else
									lhs = memory[inst.B]
								end

								if inst.is_KC then
									rhs = inst.const_C
								else
									rhs = memory[inst.C]
								end

								memory[inst.A] = lhs * rhs
							end
						elseif op > 13 then
							if op < 16 then
								if op > 14 then
									--[[TAILCALL]]
									local A = inst.A
									local B = inst.B
									local params

									if B == 0 then
										params = top_index - A
									else
										params = B - 1
									end

									close_lua_upvalues(open_list, 0)

									return memory[A](table.unpack(memory, A + 1, A + params))
								else
									--[[SETTABLE]]
									local index, value

									if inst.is_KB then
										index = inst.const_B
									else
										index = memory[inst.B]
									end

									if inst.is_KC then
										value = inst.const_C
									else
										value = memory[inst.C]
									end

									memory[inst.A][index] = value
								end
							elseif op > 16 then
								--[[NEWTABLE]]
								memory[inst.A] = {}
							else
								--[[DIV]]
								local lhs, rhs

								if inst.is_KB then
									lhs = inst.const_B
								else
									lhs = memory[inst.B]
								end

								if inst.is_KC then
									rhs = inst.const_C
								else
									rhs = memory[inst.C]
								end

								memory[inst.A] = lhs / rhs
							end
						else
							--[[LOADK]]
							memory[inst.A] = inst.const
						end
					else
						--[[FORLOOP]]
						local A = inst.A
						local step = memory[A + 2]
						local index = memory[A] + step
						local limit = memory[A + 1]
						local loops

						if step == math.abs(step) then
							loops = index <= limit
						else
							loops = index >= limit
						end

						if loops then
							memory[inst.A] = index
							memory[inst.A + 3] = index
							pc = pc + inst.sBx
						end
					end
				elseif op > 18 then
					if op < 28 then
						if op < 23 then
							if op < 20 then
								--[[LEN]]
								memory[inst.A] = #memory[inst.B]
							elseif op > 20 then
								if op < 22 then
									--[[RETURN]]
									local A = inst.A
									local B = inst.B
									local len

									if B == 0 then
										len = top_index - A + 1
									else
										len = B - 1
									end

									close_lua_upvalues(open_list, 0)

									return table.unpack(memory, A, A + len - 1)
								else
									--[[CONCAT]]
									local str = memory[inst.B]

									for i = inst.B + 1, inst.C do
										str = str .. memory[i]
									end

									memory[inst.A] = str
								end
							else
								--[[MOD]]
								local lhs, rhs

								if inst.is_KB then
									lhs = inst.const_B
								else
									lhs = memory[inst.B]
								end

								if inst.is_KC then
									rhs = inst.const_C
								else
									rhs = memory[inst.C]
								end

								memory[inst.A] = lhs % rhs
							end
						elseif op > 23 then
							if op < 26 then
								if op > 24 then
									--[[CLOSE]]
									close_lua_upvalues(open_list, inst.A)
								else
									--[[EQ]]
									local lhs, rhs

									if inst.is_KB then
										lhs = inst.const_B
									else
										lhs = memory[inst.B]
									end

									if inst.is_KC then
										rhs = inst.const_C
									else
										rhs = memory[inst.C]
									end

									if (lhs == rhs) == (inst.A ~= 0) then
										pc = pc + code[pc].sBx
									end

									pc = pc + 1
								end
							elseif op > 26 then
								--[[LT]]
								local lhs, rhs

								if inst.is_KB then
									lhs = inst.const_B
								else
									lhs = memory[inst.B]
								end

								if inst.is_KC then
									rhs = inst.const_C
								else
									rhs = memory[inst.C]
								end

								if (lhs < rhs) == (inst.A ~= 0) then
									pc = pc + code[pc].sBx
								end

								pc = pc + 1
							else
								--[[POW]]
								local lhs, rhs

								if inst.is_KB then
									lhs = inst.const_B
								else
									lhs = memory[inst.B]
								end

								if inst.is_KC then
									rhs = inst.const_C
								else
									rhs = memory[inst.C]
								end

								memory[inst.A] = lhs ^ rhs
							end
						else
							--[[LOADBOOL]]
							memory[inst.A] = inst.B ~= 0

							if inst.C ~= 0 then
								pc = pc + 1
							end
						end
					elseif op > 28 then
						if op < 33 then
							if op < 30 then
								--[[LE]]
								local lhs, rhs

								if inst.is_KB then
									lhs = inst.const_B
								else
									lhs = memory[inst.B]
								end

								if inst.is_KC then
									rhs = inst.const_C
								else
									rhs = memory[inst.C]
								end

								if (lhs <= rhs) == (inst.A ~= 0) then
									pc = pc + code[pc].sBx
								end

								pc = pc + 1
							elseif op > 30 then
								if op < 32 then
									--[[CLOSURE]]
									local sub = subs[inst.Bx + 1] -- offset for 1 based index
									local nups = sub.num_upval
									local uvlist

									if nups ~= 0 then
										uvlist = {}

										for i = 1, nups do
											local pseudo = code[pc + i - 1]

											if pseudo.op == OPCODE_RM[0] then -- @MOVE
												uvlist[i - 1] = open_lua_upvalue(open_list, pseudo.B, memory)
											elseif pseudo.op == OPCODE_RM[4] then -- @GETUPVAL
												uvlist[i - 1] = upvals[pseudo.B]
											end
										end

										pc = pc + nups
									end

									memory[inst.A] = lua_wrap_state(sub, env, uvlist)
								else
									--[[TESTSET]]
									local A = inst.A
									local B = inst.B

									if not memory[B] == (inst.C ~= 0) then
										pc = pc + 1
									else
										memory[A] = memory[B]
									end
								end
							else
								--[[UNM]]
								memory[inst.A] = -memory[inst.B]
							end
						elseif op > 33 then
							if op < 36 then
								if op > 34 then
									--[[VARARG]]
									local A = inst.A
									local len = inst.B

									if len == 0 then
										len = vararg.len
										top_index = A + len - 1
									end

									table.move(vararg.list, 1, len, A, memory)
								else
									--[[FORPREP]]
									local A = inst.A
									local init, limit, step

									init = assert(tonumber(memory[A]), "'for' initial value must be a number")
									limit = assert(tonumber(memory[A + 1]), "'for' limit must be a number")
									step = assert(tonumber(memory[A + 2]), "'for' step must be a number")

									memory[A] = init - step
									memory[A + 1] = limit
									memory[A + 2] = step

									pc = pc + inst.sBx
								end
							elseif op > 36 then
								--[[SETLIST]]
								local A = inst.A
								local C = inst.C
								local len = inst.B
								local tab = memory[A]
								local offset

								if len == 0 then
									len = top_index - A
								end

								if C == 0 then
									C = inst[pc].value
									pc = pc + 1
								end

								offset = (C - 1) * FIELDS_PER_FLUSH

								table.move(memory, A, A + len - 1, offset + 1, tab)
							else
								--[[NOT]]
								memory[inst.A] = not memory[inst.B]
							end
						else
							--[[TEST]]
							if not memory[inst.A] == (inst.C ~= 0) then
								pc = pc + 1
							end
						end
					else
						--[[TFORLOOP]]
						local A = inst.A
						local func = memory[A]
						local first = memory[A + 1]
						local second = memory[A + 2]
						local base = A + 3

						memory[base + 2] = second
						memory[base + 1] = first
						memory[base] = func

						local vals = { func(first, second) }

						table.move(vals, 1, inst.C, base, memory)

						if memory[base] ~= nil then
							memory[A + 2] = memory[base]
						else
							pc = pc + 1
						end
					end
				else
					--[[JMP]]
					pc = pc + inst.sBx
				end

				state.pc = pc
			end
		end

		function lua_wrap_state(proto, env, upval)
			local function wrapped(...)
				local passed = table.pack(...)
				local memory = table.create(proto.max_stack)
				local vararg = { len = 0, list = {} }

				table.move(passed, 1, proto.num_param, 0, memory)

				if proto.num_param < passed.n then
					local start = proto.num_param + 1
					local len = passed.n - proto.num_param

					vararg.len = len
					table.move(passed, start, start + len - 1, 1, vararg.list)
				end

				local state = { vararg = vararg, memory = memory, code = proto.code, subs = proto.subs, pc = 1 }

				local result = table.pack(pcall(run_lua_func, state, env, upval))

				if result[1] then
					return table.unpack(result, 2, result.n)
				else
					local failed = { pc = state.pc, source = proto.source, lines = proto.lines }

					on_lua_error(failed, result[2])

					return
				end
			end

			return wrapped
		end

		FiOne = function(bytecode, env)
			return lua_wrap_state(lua_bc_to_state(bytecode), env or top_env)
		end
	end

	LuaX:init()
	local LuaState = {}

	loadstring = function(str, env)
		local f, writer, buff, name
		local ran, error = pcall(function()
			local zio = LuaZ:init(LuaZ:make_getS(str), nil)
			if not zio then
				return error()
			end
			local func = LuaY:parser(LuaState, zio, nil, "@virtual")
			writer, buff = LuaU:make_setS()
			LuaU:dump(LuaState, func, writer, buff)

			f = FiOne(buff.data, env or getfenv())
		end)

		if ran then
			return f, buff.data
		else
			return nil, error
		end
	end
end

-- Constants:
local URL = "http://localhost:9999/api/receive"
local REFRESH = 0.1

-- Private Functions:
local function IsHttpEnabled(init: boolean?)
	local success, response = pcall(function()
		HttpService:GetAsync(URL)
	end)

	if init and not success then
		print(response)
	end

	return success
end

-- Init:
IsHttpEnabled(true)

while not IsHttpEnabled() do
	task.wait(1)
end

while true do
	local success, response = pcall(function()
		return HttpService:GetAsync(URL)
	end)

	if success and response ~= "" then
		local ran, error = pcall(function()
			local f = loadstring(response)
			f()
		end)

		if not ran then
			print(error)
		end
	end

	task.wait(REFRESH)
end
